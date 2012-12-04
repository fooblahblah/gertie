package org.fooblahblah.gertie

import scala.util.parsing.combinator.RegexParsers

class IRCParser extends RegexParsers {
  override def skipWhitespace = false

  def prefix: Parser[String] = """:[^ ]+ """.r

  def params: Parser[String] = """ :?(.*)""".r

  def code: Parser[CODE] = """([0-9]{3})""".r ~ params ^^ {
    case _ => CODE("123", Nil)
  }

  def unknown: Parser[UNKNOWN] = """[a-zA-Z]+""".r ~ opt("""\s+.*""".r) ^^ {
    case cmd ~ args => UNKNOWN(cmd.trim, args.map(_.trim))
  }

  def command: Parser[Command] = (
    pass
    | nick
    | user
    | quit
    | away
    | mode
    | join
    | list
    | part
    | who
    | topic
    | unknown
  )

  def whitespace = """\s+""".r

  def message: Parser[Command] = opt(prefix) ~> (command | code)

  def hostname: Parser[String] = repsep("""[a-zA-Z0-9\-]+""".r, ".") ^^ { _.mkString(".") }

  def servername = hostname

  def nickname = """([^ ]+)""".r

  def channel: Parser[String] = """(#|&)""".r ~> """[^\s,]+""".r

  def channels: Parser[Seq[String]] = repsep(channel, """,\s*""".r)

  def quit: Parser[QUIT] = "QUIT" ~> opt(" :" ~> """(.*)""".r) ^^ { QUIT(_) }

  def mode: Parser[MODE] = "MODE " ~> (channel | nickname) ~ (whitespace ~> opt("+" | "-") ~> ("o" | "p" | "s" | "i" | "t" | "n" | "b" | "v")) <~ """\s*.*""".r ^^ {
    case target ~ spec => MODE(target, spec)
  }

  def away: Parser[AWAY] = "AWAY" ~> opt(" :" ~> """(.*)""".r) ^^ { AWAY(_) }

  def pass: Parser[PASS] = "PASS " ~> """(.*)""".r ^^ { PASS(_) }

  def nick: Parser[NICK] = "NICK " ~> nickname <~ opt(""".*""".r) ^^ { NICK(_) }

  def user: Parser[USER] = "USER " ~> """[^\s]+""".r ~ " " ~ hostname ~ " " ~ servername ~ " :" ~ """.+""".r ^^ {
    case username ~ _ ~ host ~ _ ~ server ~ _ ~ real => USER(username, host, server, real)
  }

  def join: Parser[JOIN] = "JOIN " ~> channels ~ opt(whitespace ~> repsep("""[^,]+""".r, """,\s*""".r)) ^^ { case chans ~ keys =>
    def padding = 1 to (chans.length - keys.getOrElse(Nil).length) map (i => None)

    val paddedKeys = keys map { k =>
      k.map(Some(_)) ++ padding
    } getOrElse(padding)

    JOIN(chans.zip(paddedKeys))
  }

  def list: Parser[LIST] = "LIST" ~> opt(whitespace ~> channels) ^^ { LIST(_) }

  def part: Parser[PART] = "PART" ~> whitespace ~> channels ^^ { PART(_) }

  def topic: Parser[TOPIC] = "TOPIC" ~> whitespace ~> channel ~ opt(whitespace ~> opt(":") ~> """.+""".r) ^^ { case chan ~ title => TOPIC(chan, title) }

  def who: Parser[WHO] = "WHO" ~> opt(whitespace ~> channel) ^^ { WHO(_) }
}


sealed abstract class Command

case class UNKNOWN(cmd: String, params: Option[String]) extends Command

case class CODE(code: String, params: Seq[String]) extends Command

case class QUIT(msg: Option[String]) extends Command

case class AWAY(msg: Option[String]) extends Command

case class MODE(target: String, modeSpec: String) extends Command

case class PASS(password: String) extends Command

case class NICK(nick: String) extends Command

case class USER(userName: String, hostName: String, serverName: String, realName: String) extends Command

case class JOIN(channels: Seq[(String, Option[String])]) extends Command

case class LIST(channels: Option[Seq[String]]) extends Command

case class WHO(channel: Option[String]) extends Command

case class PART(channels: Seq[String]) extends Command

case class TOPIC(channel: String, title: Option[String]) extends Command
