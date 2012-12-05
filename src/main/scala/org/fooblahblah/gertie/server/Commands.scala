package org.fooblahblah.gertie.server

import spray.io.Command
import org.fooblahblah.gertie.parser._
import IRCCommands.{Command => IRCCommand}

object Commands {
  case class WrappedCommand(parseCmd: IRCCommand) extends Command
  implicit def parseCmdtoSpray(cmd: IRCCommand): Command = WrappedCommand(cmd)

  val CRLF = "\r\n"

  case object PONG extends Command {
    override def toString() = "PONG"
  }
}