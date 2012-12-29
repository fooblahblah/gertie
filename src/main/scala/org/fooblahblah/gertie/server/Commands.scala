package org.fooblahblah.gertie.server

import spray.io.Command
import org.fooblahblah.bivouac.model.Model._
import org.fooblahblah.gertie.parser._
import IRCCommands._

object Commands {
  val CRLF = "\r\n"

  val ENTER_MSG     = "EnterMessage"
  val LEAVE_MSG     = "LeaveMessage"
  val KICK_MSG      = "KickMessage"
  val PASTE_MSG     = "PasteMessage"
  val TEXT_MSG      = "TextMessage"
  val TIMESTAMP_MSG = "TimestampMessage"

  case class WrappedCommand(parseCmd: IRCCommand) extends Command

  implicit def parseCmdtoSpray(cmd: IRCCommand): Command = WrappedCommand(cmd)

  case class IRCContext(user: User, host: String, nick: String, username:  String, domain: String)

  trait Reply extends Command

  case object INITIATE_CONNECTION

  case object CHANNEL_CACHE


  case object PONG extends Reply {
    override def toString() = s"PONG${CRLF}"
  }

  case class CampfireReply(command: String, username: String, msg: String) extends Reply {
    override def toString() = s":${username}!${username}@campfire ${command.toUpperCase} ${msg}${CRLF}"
  }

  case class CommandReply(prefix: String, msg: String) extends Reply {
    override def toString() = s":gertie :${prefix.toUpperCase} :${msg}${CRLF}"
  }

  case class NumericReply(code: Int, target: String, msg: String = "") extends Reply {
    override def toString() = s":gertie ${code} ${target} ${msg}${CRLF}"
  }

  case class RawReply(msg: String) extends Reply {
    override def toString() = s"${msg}$CRLF"
  }

  case class UserReply(user: String, nick: String, host: String, command: String, msg: String) extends Reply {
    override def toString() = s":${nick}!${user}@${host} ${command.toUpperCase()} ${msg}$CRLF"
  }


  object Replies {
    val WELCOME       = 001

    val RPL_WHOREPLY  = 352
    val RPL_NAMREPLY  = 353
    val RPL_ENDOFWHO  = 315

    val RPL_LISTSTART = 321
    val RPL_LIST      = 322
    val RPL_LISTEND   = 323

    val RPL_NO_TOPIC  = 331
    val RPL_TOPIC     = 332

    val MOTDSTART     = 375
    val MOTD          = 372
    val ENDOFMOTD     = 376

  }


  object Errors {
    val NEEDMOREPARAMS     = 461
    val ERR_PASSWDMISMATCH = 464
  }
}
