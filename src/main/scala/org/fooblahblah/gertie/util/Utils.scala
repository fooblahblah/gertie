package org.fooblahblah.gertie.util

object Utils {
  /**
   * Poached directly from camper_van.
   * https://github.com/fooblahblah/camper_van/blob/master/lib/camper_van/utils.rb#L4
   */
  def ircName(name: String) = {
    name.replaceAll("/", "-").
    replaceAll("""\W""", " ").
    replaceAll("""([A-Z]+)([A-Z][a-z])""", """$1_$2""").
    replaceAll("""([a-z\d])([A-Z])""", """$1_$2""").
    replaceAll("""\s+""", "_").
    replaceAll("""\-""", "_").
    toLowerCase
  }
}