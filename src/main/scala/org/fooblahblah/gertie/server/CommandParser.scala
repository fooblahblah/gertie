package org.fooblahblah.gertie.server

import akka.event.LoggingAdapter
import spray.io._
import spray.util._
import org.fooblahblah.gertie.parser._

object CommandParser {
  import Commands._

  def apply(log: LoggingAdapter): PipelineStage = {
    new PipelineStage {
      def build(context: PipelineContext, commandPL: CPL, eventPL: EPL): Pipelines = new Pipelines {
        val parser = IRCParser

        val commandPipeline: CPL = commandPL

        val eventPipeline: EPL = {
          case x: IOPeer.Received =>
            val msg = x.buffer.array.asString.trim
            log.info(s"gertie <- " + msg)

            val cmd = parser(msg)
            commandPipeline(cmd)

          case ev =>
            eventPL(ev)
        }
      }
    }
  }
}