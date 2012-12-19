package org.fooblahblah.gertie.server

import spray.io.Command
import org.fooblahblah.gertie.parser._
import IRCCommands.{Command => IRCCommand}

object Commands {
  val CRLF = "\r\n"

  case class WrappedCommand(parseCmd: IRCCommand) extends Command

  implicit def parseCmdtoSpray(cmd: IRCCommand): Command = WrappedCommand(cmd)


  trait Reply extends Command

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
    override def toString() = s":gertie ${code} ${target} :${msg}${CRLF}"
  }

  case class RawReply(msg: String) extends Reply {
    override def toString() = s"${msg}$CRLF"
  }

  case class UserReply(user: String, nick: String, host: String, command: String, msg: String) extends Reply {
    override def toString() = s":${nick}!${user}@${host} ${command.toUpperCase()} ${msg}$CRLF"
  }


  object Replies {
    val WELCOME       = 001

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