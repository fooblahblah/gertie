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

object CommandHandler {
  import IRCCommands._
  import Commands._

  def apply(log: LoggingAdapter): PipelineStage = {
    new PipelineStage {
      def build(context: PipelineContext, commandPL: CPL, eventPL: EPL): Pipelines = new Pipelines {

        // These vars are not ideal...

        var campfireClient: Option[Bivouac] = None

        var domain:   Option[String] = None
        var host:     Option[String] = None
        var nick:     Option[String] = None
        var apiKey:   Option[String] = None
        var username: Option[String] = None

        val joinedChannels: MutableSet[String]       = MutableSet()
        val channelCache:   MutableMap[String, Room] = MutableMap()

        implicit def optStrToString(opt: Option[String]): String = opt.getOrElse("<unknown>")

        val commandPipeline: CPL = {

          case WrappedCommand(cmd) => cmd match {

            case PING =>
              commandPL(PONG)


            case NICK(str) =>
              nick = Some(str)


            case PASS(passwd) =>
              passwd.split(":").toSeq match {
                case Seq(dom, tok) =>
                  domain = Some(dom)
                  apiKey = Some(tok)

                case _ =>
                  commandPL(NumericReply(Errors.NEEDMOREPARAMS, nick, "must specify a password: subdomain:api_key"))
                  commandPL(IOPeer.Close(ConnectionCloseReasons.CleanClose))
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
                    updateNick(user)
                    sendWelcome
                    sendMOTD
                    updateChannelCache
                  } orElse {
                    commandPL(NumericReply(Errors.ERR_PASSWDMISMATCH, nick, "AUTH *** could not connect to campfire:  invalid API key. ***"))
                    commandPL(IOPeer.Close(ConnectionCloseReasons.CleanClose))
                    None
                  }
                }
              }


            case WHO(mask) =>
              commandPL(NumericReply(Replies.RPL_ENDOFWHO, nick, "End of WHO list"))


            case LIST(channels) =>
              list(channels)


            case JOIN(channels) =>
              campfireClient.map { client =>
                channels.foreach { chanPair =>
                  channelCache.get(chanPair._1) map { room =>
                    val (name, key) = chanPair
                    if(!joinedChannels.contains(name)) {
                      joinedChannels += name
                      client.join(room.id)
                    }
                  }
                }
              }


            case PART(channels) =>
              campfireClient.map { client =>
                channels.foreach { name =>
                  channelCache.get(name) map { room =>
                    if(joinedChannels.contains(name)) {
                      log.info(s"Leaving $name")
                      joinedChannels -= name
                      client.leave(room.id)
                    }
                  }
                }
              }


            case cmd  =>
              log.warning(s"Unhandled command: $cmd")
          }


          case cmd =>
            commandPL(cmd)
        }


        val eventPipeline: EPL = {
          case e: Closed =>
            campfireClient.map { client =>
              joinedChannels.foreach { name =>
                channelCache.get(name) map { room =>
                  log.info(s"Leaving $name")
                  client.leave(room.id)
                }
              }
            }
            eventPL(e)

          case e =>
            eventPL(e)
        }


        def me(): Future[Option[User]] = {
          campfireClient.map { client =>
            client.me
          } getOrElse(Future(None))
        }


        def list(channels: Option[Seq[String]]) = {
          campfireClient.foreach { client =>
            channelCache map { kv =>
              val (name, room) = kv
              commandPL(NumericReply(Replies.RPL_LIST, s"${nick} #${name} ${room.users.getOrElse(Nil).length}", room.topic))
            }
            commandPL(NumericReply(Replies.RPL_LISTEND, nick, ":End of list"))
          }
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


        def sendWelcome() {
          commandPL(NumericReply(Replies.WELCOME, nick, "Welcome to Gertie, the IRC/Campfire bridge!"))
        }


        def sendMOTD() {
          commandPL(NumericReply(Replies.MOTDSTART, nick, "- gertie Message of the day -"))
          commandPL(NumericReply(Replies.MOTD, nick, "- It's always sunny in Boulder!"))
          commandPL(NumericReply(Replies.ENDOFMOTD, nick, "End of /MOTD command"))
        }
      }
    }
  }
}