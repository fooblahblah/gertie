package org.fooblahblah.gertie.server

import akka.event.LoggingAdapter
import java.nio.ByteBuffer
import org.fooblahblah.bivouac.Bivouac
import org.fooblahblah.bivouac.model.Model._
import org.fooblahblah.gertie.parser._
import org.fooblahblah.gertie.util.Utils._
import scala.collection.mutable.{Set => MutableSet, HashMap => MutableMap}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import spray.io._
import spray.util._
import IRCCommands.{Command => IRCCommand}
import spray.io.IOBridge.Closed
import akka.actor.ActorRef
import akka.actor.PoisonPill

object CommandHandler {
  import IRCCommands._
  import Commands._

  def apply(log: LoggingAdapter): PipelineStage = {
    new PipelineStage {
      def build(context: PipelineContext, commandPL: CPL, eventPL: EPL): Pipelines = new Pipelines {

        var campfireClient: Option[Bivouac] = None

        var domain:   Option[String] = None
        var host:     Option[String] = None
        var nick:     Option[String] = None
        var apiKey:   Option[String] = None
        var username: Option[String] = None

        val joinedChannels: MutableMap[String, ActorRef] = MutableMap()
        val channelCache:   MutableMap[String, Room]     = MutableMap()

        implicit def optStrToString(opt: Option[String]): String = opt.getOrElse("<unknown>")

        val commandPipeline: CPL = {

          case WrappedCommand(cmd) => cmd match {

            case JOIN(channels) =>
              campfireClient.map { client =>
                channels.foreach { chanPair =>
                  channelCache.get(chanPair._1) map { room =>
                    val (name, key) = chanPair
                    if(!joinedChannels.contains(name)) {
                      client.join(room.id) foreach { joined =>
                        val ref = client.live(room.id, handleStreamEvent(name))
                        joinedChannels += ((name, ref))
                        commandPL(UserReply(username, nick, host, "JOIN", s":#${name}"))
                        commandPipeline(WrappedCommand(TOPIC(name, None)))
                      }
                      // TODO: Send names list
                    }
                  }
                }
              }


            case LIST(channels) =>
              campfireClient.foreach { client =>
                channelCache.toList.sortBy(_._1) foreach { kv =>
                  val (name, room) = kv
                  commandPL(NumericReply(Replies.RPL_LIST, s"${nick} #${name} ${room.users.getOrElse(Nil).length}", room.topic))
                }
                commandPL(NumericReply(Replies.RPL_LISTEND, nick, ":End of list"))
              }


            case NICK(str) =>
              nick = Some(str)


            case PING =>
              commandPL(PONG)


            case PART(channels) =>
              campfireClient.map { client =>
                channels.foreach { name =>
                  channelCache.get(name) map { room =>
                    joinedChannels.get(name) map { ref =>
                      log.info(s"Leaving $name")
                      joinedChannels -= name
                      ref ! PoisonPill
                      client.leave(room.id)
                    }
                  }
                }
              }


            case PASS(passwd) =>
              passwd.split(":").toSeq match {
                case Seq(dom, tok) =>
                  domain = Some(dom)
                  apiKey = Some(tok)

                case _ =>
                  commandPL(NumericReply(Errors.NEEDMOREPARAMS, nick, "must specify a password: subdomain:api_key"))
                  commandPL(IOPeer.Close(ConnectionCloseReasons.CleanClose))
              }


            case PRIVMSG(channels, msg) =>
              // TODO: Handle PART 0 (leave all)
              campfireClient map { client =>
                channels foreach { channel =>
                  channelCache.get(channel) map { room =>
                    client.speak(room.id, msg)
                  }
                }
              }


            case USER(user_, host_, server, name) =>
              if(apiKey.isEmpty) {
                commandPL(CommandReply("notice", "AUTH *** must specify campfire API key as password ***"))
                commandPL(IOPeer.Close(ConnectionCloseReasons.CleanClose))
              } else {
                username       = Some(user_)
                host           = Some(host_)
                campfireClient = Some(Bivouac(domain, apiKey))

                me map { me =>
                  me.map { user =>
                    updateChannelCache
                    updateNick(user)
                    sendWelcome
                    sendMOTD
                  } orElse {
                    commandPL(NumericReply(Errors.ERR_PASSWDMISMATCH, nick, "AUTH *** could not connect to campfire:  invalid API key. ***"))
                    commandPL(IOPeer.Close(ConnectionCloseReasons.CleanClose))
                    None
                  }
                }
              }


            case TOPIC(channel, titleOpt) =>
              val target = s"$nick #${channel}"

              campfireClient map { client =>
                channelCache.get(channel) map { room =>
                  titleOpt match {
                    case Some(title) =>
                      log.info(s"Updating #${channel} topic: ${title}")
                      client.updateRoomTopic(room.id, title) map { status =>
                        if(status) {
                          channelCache += ((channel, room.copy(topic = title)))
                          commandPL(NumericReply(Replies.RPL_TOPIC, target, title))
                        }
                      }

                    case _ =>
                      commandPL(NumericReply(Replies.RPL_TOPIC, target, room.topic))
                  }
                }
              }


            case WHO(mask) =>
              commandPL(NumericReply(Replies.RPL_ENDOFWHO, nick, "End of WHO list"))


            case cmd  =>
              log.warning(s"Unhandled command: $cmd")
          }


          case cmd =>
            commandPL(cmd)
        }


        val eventPipeline: EPL = {
          case e: Closed =>
            campfireClient.map { client =>
              joinedChannels.foreach { pair =>
                val (name, ref) = pair
                channelCache.get(name) map { room =>
                  log.info(s"Leaving $name")
                  ref ! PoisonPill
                  client.leave(room.id)
                }
              }
            }
            eventPL(e)

          case e =>
            eventPL(e)
        }


        def handleStreamEvent(channel: String)(msg: Message) {
           msg.messageType match {
             case "EnterMessage" =>
               log.info(msg.toString)
               campfireClient map {client =>
                 msg.userId foreach { userId =>
                   client.user(userId) foreach { userOpt =>
                     userOpt foreach { user =>
                       channelCache.get(channel) foreach { room =>
                          room.copy(users = room.users.map(_ :+ user))
                       }
                     }
                   }
                 }
               }

             case "LeaveMessage" =>
               log.info(msg.toString)
               channelCache.get(channel) foreach { room =>
                 room.copy(users = room.users.map(_.filterNot(user => user.id == msg.userId.getOrElse(0))))
               }

             case "TextMessage" =>
               commandPL(CampfireReply("PRIVMSG", userById(msg.userId.getOrElse(0), channel), s"#${channel} :${msg.body.getOrElse("")}"))

             case _ =>
               log.info(msg.toString)
           }
        }


        def me(): Future[Option[User]] = {
          campfireClient.map { client =>
            client.me
          } getOrElse(Future(None))
        }


        def sendWelcome() {
          commandPL(NumericReply(Replies.WELCOME, nick, "Welcome to Gertie, the IRC/Campfire bridge!"))
        }


        def sendMOTD() {
          commandPL(NumericReply(Replies.MOTDSTART, nick, "- gertie Message of the day -"))
          commandPL(NumericReply(Replies.MOTD, nick, "- It's always sunny in Boulder!"))
          commandPL(NumericReply(Replies.ENDOFMOTD, nick, "End of /MOTD command"))
        }


        def updateChannelCache {
          campfireClient.foreach { client =>
            channelCache.clear
            client.rooms map { rooms =>
              rooms map { room =>
                client.room(room.id) map { room =>
                  room foreach { room =>
                    channelCache += ((ircName(room.name), room))
                  }
                }
              }
            }
          }
        }


        def updateNick(user: User) {
          val name = ircName(user.name)
          if(name != nick) {
            commandPL(RawReply(s":${nick.get}!${username.get}@${host.get} NICK ${name}"))
            nick = Some(name)
          }
        }


        def userById(userId: Int, channel: String): String = {
          channelCache.get(channel) flatMap { channel =>
            channel.users.flatMap { users =>
              users.filter(u => u.id == userId).headOption.map(u => ircName(u.name))
            }
          } getOrElse("unknown")
        }
      }
    }
  }
}