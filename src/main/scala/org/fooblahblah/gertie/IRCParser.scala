package org.fooblahblah.gertie

import scala.util.parsing.combinator.RegexParsers

class IRCParser extends RegexParsers {
  override def skipWhitespace = false
  
  def prefix: Parser[String] = """:[^ ]+ """.r
  
  def params: Parser[String] = """ :?(.*)""".r

  def code: Parser[CODE] = """([0-9]{3})""".r ~ params ^^ { 
    case _ => CODE("123", Nil) 
  }
  
  def unknown: Parser[UNKNOWN] = """[a-zA-Z]+ """.r ~ """.*""".r ^^ { 
    case cmd ~ args => UNKNOWN(cmd.trim, args.trim) 
  } 
    
  def command: Parser[Command] = (
    pass 
    | nick
    | user
    | quit
    | unknown
  )
  
  def hostname: Parser[String] = repsep("""[a-zA-Z0-9\-]+""".r, ".") ^^ { _.mkString(".") }
  
  def servername = hostname
  
  def message: Parser[Command] = opt(prefix) ~> (command | code)

  def quit: Parser[QUIT] = "QUIT " ~> opt(":" ~> """(.*)""".r) ^^ { QUIT(_) }

  def pass: Parser[PASS] = "PASS " ~> """(.*)""".r ^^ { PASS(_) }
  
  def nick: Parser[NICK] = "NICK " ~> """([^ ]+)""".r <~ opt(""".*""".r) ^^ { NICK(_) }

  def user: Parser[USER] = "USER " ~> """[^\s]+""".r ~ " " ~ hostname ~ " " ~ servername ~ " :" ~ """.+""".r ^^ { 
    case username ~ _ ~ host ~ _ ~ server ~ _ ~ real => USER(username, host, server, real) 
  }
}

sealed abstract class Command
case class UNKNOWN(cmd: String, params: String) extends Command
case class CODE(code: String, params: Seq[String]) extends Command
case class QUIT(msg: Option[String]) extends Command
case class PASS(password: String) extends Command
case class NICK(nick: String) extends Command
case class USER(userName: String, hostName: String, serverName: String, realName: String) extends Command
