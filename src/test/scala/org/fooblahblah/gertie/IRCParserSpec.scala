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
      parser.parseAll(message, "BLAH foo").get === UNKNOWN("BLAH", Some("foo"))
    }

    "handle unknown message with prefix" in {
      parser.parseAll(message, ":foo BLAH foo").get === UNKNOWN("BLAH", Some("foo"))
    }

    "handle QUIT" in {
      parser.parseAll(message, "QUIT").get === QUIT(None)
      parser.parseAll(message, "QUIT :audi5000").get === QUIT(Some("audi5000"))
    }

    "handle AWAY" in {
      parser.parseAll(message, "AWAY").get === AWAY(None)
      parser.parseAll(message, "AWAY :out to lunch").get === AWAY(Some("out to lunch"))
    }

    "handle MODE" in {
      parser.parseAll(message, "MODE #scala o").get === MODE("scala", "o")
      parser.parseAll(message, "MODE #scala +s").get === MODE("scala", "s")
      parser.parseAll(message, "MODE fooblahblah -i").get === MODE("fooblahblah", "i")
      parser.parseAll(message, "MODE fooblahblah -i random blah").get === MODE("fooblahblah", "i")
    }

    "handle PASS" in {
      parser.parseAll(message, "PASS foo").get === PASS("foo")
    }

    "handle NICK" in {
      parser.parseAll(message, "NICK fooblahblah").get === NICK("fooblahblah")
    }

    "handle USER" in {
      parser.parseAll(message, "USER jsimpson fugazi localhost :Jeff Simpson").get === USER("jsimpson", "fugazi", "localhost", "Jeff Simpson")
    }

    "handle JOIN" in {
      parser.parseAll(message, "JOIN #boulder").get === JOIN(List(("boulder", None)))
      parser.parseAll(message, "JOIN #boulder, &scala").get === JOIN(List(("boulder", None), ("scala", None)))
      parser.parseAll(message, "JOIN #boulder, &scala bogus123").get === JOIN(List(("boulder", Some("bogus123")), ("scala", None)))
      parser.parseAll(message, "JOIN #boulder, &scala bogus123,   fubar").get === JOIN(List(("boulder", Some("bogus123")), ("scala", Some("fubar"))))
    }

    "handle LIST" in {
      parser.parseAll(message, "LIST").get === LIST(None)
      parser.parseAll(message, "LIST #boulder").get === LIST(Some(List("boulder")))
      parser.parseAll(message, "LIST #boulder,  &chan1").get === LIST(Some(List("boulder", "chan1")))
    }

    "handle PART" in {
      parser.parseAll(message, "PART #boulder").get === PART(List("boulder"))
      parser.parseAll(message, "PART #boulder,  &chan1").get === PART(List("boulder", "chan1"))
    }

    "handle WHO" in {
      parser.parseAll(message, "WHO").get === WHO(None)
      parser.parseAll(message, "WHO #boulder").get === WHO(Some("boulder"))
    }

    "handle TOPIC" in {
      parser.parseAll(message, "TOPIC #boulder").get === TOPIC("boulder", None)
      parser.parseAll(message, "TOPIC #boulder smoke em while you got em!").get === TOPIC("boulder", Some("smoke em while you got em!"))
    }
}
}
