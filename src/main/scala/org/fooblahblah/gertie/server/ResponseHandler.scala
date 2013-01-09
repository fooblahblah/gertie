package org.fooblahblah.gertie.server

import akka.event.LoggingAdapter
import java.nio.ByteBuffer
import spray.io._
import spray.util._
import org.fooblahblah.gertie.parser._
import IRCCommands._

object ResponseHandler {
  import IRCCommands._
  import Commands._

  def apply(log: LoggingAdapter): PipelineStage = {
    new PipelineStage {
      def build(context: PipelineContext, commandPL: CPL, eventPL: EPL): Pipelines = new Pipelines {

        val commandPipeline: CPL = {
          case cmd: Reply =>
            log.debug(s"send -> $cmd")
            commandPL(IOPeer.Send(ByteBuffer.wrap(s"$cmd".getBytes), None))

          case cmd =>
            commandPL(cmd)
        }

        val eventPipeline: EPL = eventPL
      }
    }
  }
}