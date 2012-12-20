package org.fooblahblah.gertie.parser

import org.specs2.mutable._
import org.junit.runner.RunWith
import scala.Some.apply
import scala.collection.Seq.apply
import scala.collection.immutable.List.apply
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class IRCParserSpec extends Specification {

  lazy val parser = IRCParser

  import IRCCommands._

  "The parser" should {
    "handle unknown message" in {
      parser("BLAH foo") === UNKNOWN("BLAH", Some("foo"))
    }

    "handle unknown message with prefix" in {
      parser(":foo BLAH foo") === UNKNOWN("BLAH", Some("foo"))
    }

    "handle QUIT" in {
      parser("QUIT") === QUIT(None)
      parser("QUIT :audi5000") === QUIT(Some("audi5000"))
    }

    "handle PING" in {
      parser("PING") === PING
    }

    "handle lowercase command" in {
      parser("ping") === PING
    }

    "handle AWAY" in {
      parser("AWAY") === AWAY(None)
      parser("AWAY :out to lunch") === AWAY(Some("out to lunch"))
    }

    "handle MODE" in {
      parser("MODE #scala o") === MODE("scala", "o")
      parser("MODE #scala +s") === MODE("scala", "s")
      parser("MODE fooblahblah -i") === MODE("fooblahblah", "i")
      parser("MODE fooblahblah -i random blah") === MODE("fooblahblah", "i")
    }

    "handle PASS" in {
      parser("PASS foo") === PASS("foo")
    }

    "handle NICK" in {
      parser("NICK fooblahblah") === NICK("fooblahblah")
    }

    "handle USER" in {
      parser("USER jsimpson fugazi localhost :Jeff Simpson") === USER("jsimpson", "fugazi", "localhost", "Jeff Simpson")
      parser("USER fooblahblah 8 * :Jeff Simpson") === USER("fooblahblah", "8", "*", "Jeff Simpson")
    }

    "handle JOIN" in {
      parser("JOIN #boulder") === JOIN(List(("boulder", None)))
      parser("JOIN #boulder, &scala") === JOIN(List(("boulder", None), ("scala", None)))
      parser("JOIN #boulder, &scala bogus123") === JOIN(List(("boulder", Some("bogus123")), ("scala", None)))
      parser("JOIN #boulder, &scala bogus123,   fubar") === JOIN(List(("boulder", Some("bogus123")), ("scala", Some("fubar"))))
    }

    "handle LIST" in {
      parser("LIST") === LIST(None)
      parser("LIST #boulder") === LIST(Some(List("boulder")))
      parser("LIST #boulder,  &chan1") === LIST(Some(List("boulder", "chan1")))
    }

    "handle PART" in {
      parser("PART #boulder") === PART(List("boulder"))
      parser("PART #boulder,  &chan1") === PART(List("boulder", "chan1"))
    }

    "handle WHO" in {
      parser("WHO") === WHO(None)
      parser("WHO fooblahblah") === WHO(Some("fooblahblah"))
      parser("WHO #boulder") === WHO(Some("boulder"))
    }

    "handle TOPIC" in {
      parser("TOPIC #boulder") === TOPIC("boulder", None)
      parser("TOPIC #boulder smoke em while you got em!") === TOPIC("boulder", Some("smoke em while you got em!"))
    }

    "handle NAMES" in {
      parser("NAMES #boulder") === NAMES(Seq("boulder"))
      parser("NAMES   #boulder  , #scala") === NAMES(Seq("boulder", "scala"))
      parser(":random_prefix NAMES   #boulder  , #scala") === NAMES(Seq("boulder", "scala"))
    }

    "handle PRIVMSG" in {
      val msg = "This is a test"
      parser(s"PRIVMSG #boulder :$msg") === PRIVMSG(Seq("boulder"), msg)
      parser(s"PRIVMSG #boulder blah") === PRIVMSG(Seq("boulder"), "blah")
    }

    "handle HISTORY" in {
      parser("HISTORY #boulder") === HISTORY("boulder")
    }
  }
}
