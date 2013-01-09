package org.fooblahblah.gertie.parser

import scala.util.parsing.combinator.RegexParsers
import scala.Some.apply

object IRCParser extends RegexParsers {
  import IRCCommands._

  override def skipWhitespace = false

  def code: Parser[CODE] = """([0-9]{3})""".r ~ repsep(params, comma) ^^ {
    case code ~ params => CODE(code, params)
  }

  def command: Parser[IRCCommand] = (
    pass
    | away
    | history
    | join
    | join0
    | list
    | mode
    | names
    | nick
    | part
    | privmsg
    | ping
    | quit
    | topic
    | user
    | userhost
    | who
    | unknown
  )

  def message: Parser[IRCCommand] = opt(prefix) ~> (command | code)

  def newline = "\n"

  def CRLF = "\r\n"

  def letter = """[a-zA-Z]""".r

  def number = """[0-9]""".r

  def special = "-" | "_" | "[" | "]" | "\\" | "`" | "^" | "{" | "}"

  def whitespace = """\s+""".r

  def comma = """\s*,\s*""".r

  def prefix: Parser[String] = """:[^ ]+ """.r

  def params: Parser[String] = whitespace ~> ( ":" ~> """.*""".r | opt(":") ~> """.*""".r )

  def hostname: Parser[String] = repsep("""[a-zA-Z0-9\-\*]+""".r, ".") ^^ { _.mkString(".") }

  def servername = hostname

  def nickname = letter ~ rep(letter | number | special) ^^ {case leading ~ rest => leading + rest.mkString }

  def unknown: Parser[UNKNOWN] = """[a-zA-Z]+""".r ~ opt("""\s+.*""".r) ^^ {
    case cmd ~ args => UNKNOWN(cmd.trim, args.map(_.trim))
  }

  def channel: Parser[String] = """(#|&)""".r ~> """[^\s,]+""".r

  def channels: Parser[Seq[String]] = repsep(channel, comma)

  def ping = """(?i)PING.*""".r ^^ { case s => PING }

  def quit: Parser[QUIT] = "(?i)QUIT".r ~> opt(whitespace ~> ":" ~> """(.*)""".r) ^^ { QUIT(_) }

  def mode: Parser[MODE] = "(?i)MODE".r ~> whitespace ~> (channel | nickname) ~ opt(whitespace ~> opt("+" | "-") ~> ("o" | "p" | "s" | "i" | "t" | "n" | "b" | "v")) <~ """\s*.*""".r ^^ {
    case target ~ spec => MODE(target, spec)
  }

  def away: Parser[AWAY] = "(?i)AWAY".r ~> opt(whitespace ~> ":" ~> """(.*)""".r) ^^ { AWAY(_) }

  def pass: Parser[PASS] = "(?i)PASS".r ~> whitespace ~> """(.*)""".r ^^ { PASS(_) }

  def nick: Parser[NICK] = "(?i)NICK".r ~> whitespace ~> nickname <~ opt(""".*""".r) ^^ { NICK(_) }

  def user: Parser[USER] = "(?i)USER ".r ~> """[^\s]+""".r ~ whitespace ~ hostname ~ whitespace ~ servername ~ whitespace ~ opt(":") ~  """.+""".r  ^^ {
    case username ~ _ ~ host ~ _ ~ server ~ _ ~ _ ~ real => USER(username, host, server, real)
  }


  def join: Parser[JOIN] = "(?i)JOIN".r ~> whitespace ~> (channels ~ opt(whitespace ~> repsep("""[^,]+""".r, """,\s*""".r))) ^^ {
    case chans ~ keys =>
      def padding = 1 to (chans.length - keys.getOrElse(Nil).length) map (i => None)

      val paddedKeys = keys map { k =>
        k.map(Some(_)) ++ padding
      } getOrElse(padding)

      JOIN(chans.zip(paddedKeys))
  }

  def join0: Parser[JOIN] = """(?i)JOIN\s+0""".r ^^ { case _ => JOIN(Seq(("0", None))) }

  def list: Parser[LIST] = "(?i)LIST.*".r ^^ { case _ => LIST(None) }

  def part: Parser[PART] = "(?i)PART".r ~> whitespace ~> channels <~ """.*""".r ^^ { PART(_) }

  def topic: Parser[TOPIC] = "(?i)TOPIC".r ~> whitespace ~> channel ~ opt(whitespace ~> opt(":") ~> """.+""".r) ^^ { case chan ~ title => TOPIC(chan, title) }

  def who: Parser[WHO] = "(?i)WHO".r ~> opt(whitespace ~> (channel | """\S+""".r)) ^^ { WHO(_) }

  def names: Parser[NAMES] = "(?i)NAMES".r ~> whitespace ~> channels ^^ { NAMES(_) }

  def history: Parser[HISTORY] = "(?i)HISTORY".r ~> whitespace ~> channel ^^ { HISTORY(_) }

  def privmsg: Parser[PRIVMSG] = "(?i)PRIVMSG".r ~> whitespace ~> repsep(nickname | channel, comma) ~ (whitespace ~> opt(":") ~> """.+""".r) ^^ {
    case target ~ msg => PRIVMSG(target, msg)
  }

  def userhost: Parser[USERHOST] = "(?i)USERHOST".r ~> whitespace ~> repsep(nickname, """\s+""".r) ^^ { USERHOST(_) }

  def apply(buffer: String) = {
    parseAll(message, buffer) match {
      case Success(cmd, _)    => cmd
      case failure: NoSuccess => sys.error(failure.msg)
    }
  }
}


object IRCCommands {
  sealed abstract class IRCCommand

  case object PING extends IRCCommand

  case class AWAY(msg: Option[String]) extends IRCCommand

  case class CODE(code: String, params: Seq[String]) extends IRCCommand

  case class HISTORY(channel: String) extends IRCCommand

  case class JOIN(channels: Seq[(String, Option[String])]) extends IRCCommand

  case class LIST(channels: Option[Seq[String]]) extends IRCCommand

  case class MODE(target: String, modeSpec: Option[String]) extends IRCCommand

  case class NAMES(channels: Seq[String]) extends IRCCommand

  case class NICK(nick: String) extends IRCCommand

  case class PART(channels: Seq[String]) extends IRCCommand

  case class PASS(password: String) extends IRCCommand

  case class PRIVMSG(target: Seq[String], msg: String) extends IRCCommand

  case class QUIT(msg: Option[String]) extends IRCCommand

  case class TOPIC(channel: String, title: Option[String]) extends IRCCommand

  case class UNKNOWN(cmd: String, params: Option[String]) extends IRCCommand

  case class USER(userName: String, hostName: String, serverName: String, realName: String) extends IRCCommand

  case class USERHOST(nicknames: Seq[String]) extends IRCCommand

  case class WHO(channel: Option[String]) extends IRCCommand
}
