package org.fooblahblah.gertie

import scala.util.parsing.combinator.RegexParsers

class IRCParser extends RegexParsers {
  override def skipWhitespace = false
  
  def prefix: Parser[String] = """:[^ ]+ """.r
  
  def params: Parser[String] = """ :?(.*)""".r

  def code: Parser[Code] = """([0-9]{3})""".r ~ params ^^ { 
    case _ => Code("123", Nil) 
  }
  
  def unknown: Parser[Unknown] = """[a-zA-Z]+ """.r ~ """.*""".r ^^ { 
    case cmd ~ args => Unknown(cmd.trim, args.trim) 
  } 
    
  def command: Parser[Command] = (
    pass 
    | nick
    | user
    | unknown
  )
  
  def hostname: Parser[String] = """[a-zA-Z0-9\-]+""".r
  
  def servername = hostname
  
  def message: Parser[Command] = opt(prefix) ~> (command | code)

  def pass: Parser[PASS] = "PASS " ~> """(.*)""".r ^^ { PASS(_) }
  
  def nick: Parser[NICK] = "NICK " ~> """([^ ]+)""".r <~ opt(""".*""".r) ^^ { NICK(_) }

  def user: Parser[USER] = "USER " ~> """[^\s]+""".r ~ " " ~ hostname ~ " " ~ servername ~ " :" ~ """.+""".r ^^ { 
    case username ~ _ ~ host ~ _ ~ server ~ _ ~ real => USER(username, host, server, real) 
  }
}

sealed abstract class Command
case class Unknown(cmd: String, params: String) extends Command
case class Code(code: String, params: Seq[String]) extends Command
case class PASS(password: String) extends Command
case class NICK(nick: String) extends Command
case class USER(userName: String, hostName: String, serverName: String, realName: String) extends Command
