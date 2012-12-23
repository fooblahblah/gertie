package org.fooblahblah.gertie.server

import akka.actor._
import akka.event.Logging
import org.fooblahblah.bivouac.Bivouac
import org.fooblahblah.bivouac.model.Model._
import org.fooblahblah.gertie.parser._
import org.fooblahblah.gertie.util.Utils._
import spray.io._
import spray.util._
import scala.collection.mutable.{Set => MutableSet, HashMap => MutableMap}
import scala.concurrent.Future
import IRCCommands._
import Commands._

class IRCSessionProtocol(
    context:    PipelineContext,
    commandPL:  (Command) => Unit,
    client:     Bivouac,
    ircContext: IRCContext)(implicit system: ActorSystem) extends Actor {

  import ircContext._

  val log = Logging(system, this)

  val channelCache:    MutableMap[String, Room]     = MutableMap()
  val joinedChannels:  MutableMap[String, ActorRef] = MutableMap()
  val uidToNameCache:  MutableMap[Int, String]      = MutableMap()


  def receive = {
    case INITIATE_CONNECTION =>
      updateChannelCache map { _ =>
        sendWelcome
        sendMOTD
      }


    case CHANNEL_CACHE =>
      sender ! (channelCache.toMap, joinedChannels.toMap)


    case HISTORY(channel) =>
      channelCache.get(channel) foreach { room =>
        client.recentMessages(room.id) map { l =>
          val msgs = l.filterNot(msg => msg.messageType == "EnterMessage" || msg.messageType == "LeaveMessage")

          def processMsgs(msgs: List[Message]): Future[Unit] = msgs match {
            case msg :: ms => handleStreamEvent(channel)(msg) flatMap(_ => processMsgs(ms))
            case _         => Future.successful()
          }

          processMsgs(msgs)
        }
      }


    case JOIN(channels) =>
      channels.foreach { chanPair =>
        val (chanName, _) = chanPair

        channelCache.get(chanName) map { room =>
          if(!joinedChannels.contains(chanName)) {
            client.join(room.id) foreach { joined =>
              val ref = client.live(room.id, handleStreamEvent(chanName))
              joinedChannels += ((chanName, ref))

              commandPL(UserReply(username, nick, host, "JOIN", s":#${chanName}"))
              self ! TOPIC(chanName, None)

              room.users.map(_.map(u => ircName(u.name)).mkString(" ")) map { nickList =>
                commandPL(NumericReply(Replies.RPL_NAMREPLY, nick, s"= #${chanName} :${nickList}"))
              }

              self ! HISTORY(chanName)
            }
          }
        }
      }


    case LIST(channels) =>
      channelCache.toList.sortBy(_._1) foreach { kv =>
        val (name, room) = kv
        commandPL(NumericReply(Replies.RPL_LIST, nick, s"#${name} ${room.users.getOrElse(Nil).length} :${room.topic}"))
      }
      commandPL(NumericReply(Replies.RPL_LISTEND, nick, ":End of list"))


    case PING =>
      commandPL(PONG)


    case PART(channels) =>
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


    case PRIVMSG(channels, msg) =>
      // TODO: Handle PART 0 (leave all)
      channels foreach { channel =>
        channelCache.get(channel) map { room =>
          client.speak(room.id, msg)
        }
      }




    case TOPIC(channel, titleOpt) =>
      val target = s"$nick #${channel}"
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


  def handleStreamEvent(channel: String)(msg: Message): Future[Unit] = {

    msg.messageType match {

      case "EnterMessage" =>
        log.info(msg.toString)
        Future.successful(msg.userId foreach { userId =>
          client.user(userId) foreach { userOpt =>
            userOpt foreach { user =>
              channelCache.get(channel) foreach { room =>
                log.info(s"${user.name} entered room")
                channelCache += ((channel, room.copy(users = room.users.map(_ :+ user))))
              }
            }
          }
        })


      case "KickMessage" =>
        Future.successful()


      case "LeaveMessage" =>
        log.info(msg.toString)
        Future.successful(channelCache.get(channel) foreach { room =>
          val userId = msg.userId.getOrElse(0)
          log.info(s"${userById(userId, channel)} left room")
          channelCache += ((channel, room.copy(users = room.users.map(_.filterNot(user => user.id == userId)))))
        })


      case "PasteMessage" =>
        val lines = msg.body.map(_.split("\n")).getOrElse(sys.error("No body for PasteMessage"))

        userById(msg.userId.getOrElse(0), channel) map { nick =>
          lines.take(3) foreach { line =>
            commandPL(CampfireReply("PRIVMSG", nick, s"#${channel} :> ${line}"))
          }

          if(lines.length > 3)
            commandPL(CampfireReply("PRIVMSG", nick, s"#${channel} :> more: https://${domain}.campfirenow.com/room/${msg.roomId}/paste/${msg.id}"))
        }

      case "TextMessage" =>
        if(user.id != msg.userId) {
          userById(msg.userId.getOrElse(0), channel) map { nick =>
            commandPL(CampfireReply("PRIVMSG", nick, s"#${channel} :${msg.body.getOrElse("")}"))
          }
        } else Future.successful()


      case "TimestampMessage" =>
        Future.successful()


      case _ =>
        log.info(msg.toString)
        if(user.id != msg.userId) {
          userById(msg.userId.getOrElse(0), channel) map { nick =>
            commandPL(CampfireReply("PRIVMSG", nick, s"#${channel} :${msg.body.getOrElse("")}"))
          }
        } else Future.successful()
    }
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
  }


  def userById(userId: Int, channel: String): Future[String] = {
    uidToNameCache.get(userId) map (Future.successful(_)) getOrElse {
      client.user(userId) map { opt =>
        opt map { user =>
          val name = ircName(user.name)
          uidToNameCache += ((userId, name))
          name
        } getOrElse(userId.toString)
      }
    }
  }
}