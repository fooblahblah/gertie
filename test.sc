package org.fooblahblah.gertie

object test extends IRCParser {
	parseAll(pass, "PASS foo") match {
		case Success(s, _) => println(s)
		case x => println(x)
	}
}