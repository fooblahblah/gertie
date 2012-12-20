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
        var myUserId: Option[Int]    = None

        val joinedChannels: MutableMap[String, ActorRef] = MutableMap()
        val channelCache:   MutableMap[String, Room]     = MutableMap()

        implicit def optStrToString(opt: Option[String]): String = opt.getOrElse("<unknown>")

        val commandPipeline: CPL = {

          case WrappedCommand(cmd) => cmd match {

            case HISTORY(channel) =>
              campfireClient foreach { client =>
                channelCache.get(channel) foreach { room =>
                  client.recentMessages(room.id) foreach {
                    _ foreach { msg =>
                      handleStreamEvent(channel)(msg)
                    }
                  }
                }
              }


            case JOIN(channels) =>
              campfireClient.map { client =>
                channels.foreach { chanPair =>
                  val (chanName, key) = chanPair
                  channelCache.get(chanName) map { room =>
                    if(!joinedChannels.contains(chanName)) {
                      client.join(room.id) foreach { joined =>
                        val ref = client.live(room.id, handleStreamEvent(chanName))
                        joinedChannels += ((chanName, ref))

                        commandPL(UserReply(username, nick, host, "JOIN", s":#${chanName}"))
                        commandPipeline(WrappedCommand(TOPIC(chanName, None)))

                        room.users.map(_.map(u => ircName(u.name)).mkString(" ")) map { nickList =>
                          commandPL(NumericReply(Replies.RPL_NAMREPLY, nick, s"= #${chanName} :${nickList}"))
                        }

                        commandPipeline(WrappedCommand(HISTORY(chanName)))
                      }
                    }
                  }
                }
              }


            case LIST(channels) =>
              campfireClient.foreach { client =>
                channelCache.toList.sortBy(_._1) foreach { kv =>
                  val (name, room) = kv
                  commandPL(NumericReply(Replies.RPL_LIST, nick, s"#${name} ${room.users.getOrElse(Nil).length} :${room.topic}"))
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
                  commandPL(NumericReply(Errors.NEEDMOREPARAMS, nick, ":must specify a password: subdomain:api_key"))
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
                host           = Some(context.connection.remoteAddress.getHostName())
                campfireClient = Some(Bivouac(domain, apiKey))

                me map { me =>
                  me.map { user =>
                    myUserId = Some(user.id)
                    updateChannelCache map { _ =>
                      updateNick(user)
                      sendWelcome
                      sendMOTD
                    }
                  } orElse {
                    commandPL(NumericReply(Errors.ERR_PASSWDMISMATCH, nick, ":AUTH *** could not connect to campfire:  invalid API key. ***"))
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
                          commandPL(NumericReply(Replies.RPL_TOPIC, target, s":${title}"))
                        }
                      }

                    case _ =>
                      commandPL(NumericReply(Replies.RPL_TOPIC, target, s":${room.topic}"))
                  }
                }
              }


            case WHO(channelOpt) =>
              channelOpt map { channel =>
                channelCache.get(channel) map { room =>
                  room.users map {
                    _.foreach { user =>
                      val (account, domain) = user.email.split("@").toList match {
                        case account :: domain :: Nil => (account, domain)
                        case _                        => ("unknown", "unknown")
                      }
                      commandPL(NumericReply(Replies.RPL_WHOREPLY, nick, s"#${channel} ${account} ${domain} gertie ${ircName(user.name)} H@ :0 #${user.name}"))
                    }
                  }
                }
              }
              commandPL(NumericReply(Replies.RPL_ENDOFWHO, nick, ":End of WHO list"))


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
                        log.info(s"${user.name} entered room")
                        channelCache += ((channel, room.copy(users = room.users.map(_ :+ user))))
                      }
                    }
                  }
                }
              }

            case "LeaveMessage" =>
              log.info(msg.toString)
              channelCache.get(channel) foreach { room =>
                val userId = msg.userId.getOrElse(0)
                log.info(s"${userById(userId, channel)} left room")
                channelCache += ((channel, room.copy(users = room.users.map(_.filterNot(user => user.id == userId)))))
              }

            case "TextMessage" =>
              if(myUserId != msg.userId)
                commandPL(CampfireReply("PRIVMSG", userById(msg.userId.getOrElse(0), channel), s"#${channel} :${msg.body.getOrElse("")}"))

            case "KickMessage" => //NOOP

            case "TimestampMessage" => //NOOP

            case _ =>
              log.info(msg.toString)
              if(myUserId != msg.userId)
                commandPL(CampfireReply("PRIVMSG", userById(msg.userId.getOrElse(0), channel), s"#${channel} :${msg.body.getOrElse("")}"))
          }
        }


        def me(): Future[Option[User]] = {
          campfireClient.map { client =>
            client.me
          } getOrElse(Future(None))
        }


        def sendWelcome() {
          commandPL(NumericReply(Replies.WELCOME, nick, ":Welcome to Gertie, the IRC/Campfire bridge!"))
        }


        def sendMOTD() {
          commandPL(NumericReply(Replies.MOTDSTART, nick, ":- gertie Message of the day -"))
          commandPL(NumericReply(Replies.MOTD, nick, ":- It's always sunny in Boulder!"))
          commandPL(NumericReply(Replies.ENDOFMOTD, nick, ":End of /MOTD command"))
        }


        def updateChannelCache: Future[Unit] = {
          campfireClient map { client =>
            channelCache.clear
            client.rooms map { rooms =>
              rooms foreach { room =>
                client.room(room.id) map { room =>
                  room foreach { room =>
                    channelCache += ((ircName(room.name), room))
                  }
                }
              }
            }
          } getOrElse(Future())
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
          } getOrElse(userId.toString)
        }
      }
    }
  }
}