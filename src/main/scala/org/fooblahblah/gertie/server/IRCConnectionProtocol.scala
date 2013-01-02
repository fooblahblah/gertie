package org.fooblahblah.gertie.server

import akka.actor._
import akka.pattern._
import akka.util.Timeout
import org.fooblahblah.bivouac.Bivouac
import org.fooblahblah.bivouac.model.Model._
import org.fooblahblah.gertie.parser._
import org.fooblahblah.gertie.util.Utils._
import scala.concurrent.duration._
import spray.io._
import spray.util._
import IRCCommands._

class IRCConnectionProtocol(context: PipelineContext, commandPL: (Command) => Unit)(implicit system: ActorSystem) extends Actor {
  import IRCCommands._
  import Commands._

  implicit val timeout = Timeout(5 seconds)

  var apiKey: Option[String] = None
  var domain: Option[String] = None
  var nick:   Option[String] = None

  var sessionRef: Option[ActorRef] = None

  lazy val client: Bivouac = {
    domain flatMap { dom =>
      apiKey map { key =>
        Bivouac(dom, key)
      }
    } getOrElse {
      commandPL(NumericReply(Errors.NEEDMOREPARAMS, "unknown", ":must specify a password: subdomain:api_key"))
      commandPL(IOPeer.Close(ConnectionCloseReasons.CleanClose))
      sys.error("Domain and apiKey must be defined before using the campfire client")
    }
  }

  def receive = {

    case NICK(str) =>
      nick = Some(str)


    case PASS(passwd) =>
      passwd.split(":").toSeq match {
        case Seq(dom, tok) =>
          domain = Some(dom)
          apiKey = Some(tok)

        case _ =>
          commandPL(NumericReply(Errors.NEEDMOREPARAMS, "unknown", ":must specify a password: subdomain:api_key"))
          commandPL(IOPeer.Close(ConnectionCloseReasons.CleanClose))
      }


      case USER(username, host, server, name) =>
        domain flatMap { dom =>
          apiKey map { key =>
            val client = Bivouac(dom, key)

            client.me map { me =>
              me.map { user =>
                val host = context.connection.remoteAddress.getHostName()
                val newNick = ircName(user.name)

                commandPL(RawReply(s":${newNick}!${username}@${host} NICK ${newNick}"))

                sessionRef.foreach(_ ! PoisonPill)

                val ircContext = new IRCContext(user, host, newNick, username, domain.getOrElse("unknown"))
                val ref = system.actorOf(Props(new IRCSessionProtocol(context, commandPL, client, ircContext)))

                ref ! INITIATE_CONNECTION

                sessionRef = Some(ref)
              } orElse {
                commandPL(NumericReply(Errors.ERR_PASSWDMISMATCH, "unknown", ":AUTH *** could not connect to campfire:  invalid API key. ***"))
                commandPL(IOPeer.Close(ConnectionCloseReasons.CleanClose))
                None
              }
            }
          }
        } getOrElse {
          commandPL(NumericReply(Errors.NEEDMOREPARAMS, "unknown", ":must specify a password: subdomain:api_key"))
          commandPL(IOPeer.Close(ConnectionCloseReasons.CleanClose))
          sys.error("Domain and apiKey must be defined before using the campfire client")
        }


    case cmd =>
      sessionRef.foreach { ref =>
        ref ? cmd pipeTo sender
      }
  }
}
