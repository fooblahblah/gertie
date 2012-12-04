package org.fooblahblah.gertie

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class IRCParserSpec extends Specification {

  lazy val parser = new IRCParser
  
  import parser._
  
  "The parser" should {
    "handle unknown message" in {
      val result = parser.parseAll(message, "BLAH foo")
      result.get === Unknown("BLAH", "foo")
    }
    
    "handle unknown message with prefix" in {
      val result = parser.parseAll(message, ":foo BLAH foo")
      result.get === Unknown("BLAH", "foo")
    }
    
    "handle PASS" in {
      val result = parser.parseAll(message, "PASS foo")
      result.get === PASS("foo")
    }

    "handle NICK" in {
      val result = parser.parseAll(message, "NICK fooblahblah")
      result.get === NICK("fooblahblah")
    }
    
    "handle USER" in {
      val result = parser.parseAll(message, "USER jsimpson fugazi localhost :Jeff Simpson")
      result.get === USER("jsimpson", "fugazi", "localhost", "Jeff Simpson")
    }
  }
}
