package org.fooblahblah.gertie.server

import akka.event.LoggingAdapter
import spray.io._
import spray.util._
import org.fooblahblah.gertie.parser._
import IRCCommands.{Command => IRCCommand}
import java.nio.ByteBuffer

object ResponseHandler {
  import IRCCommands._
  import Commands._

  def apply(log: LoggingAdapter): PipelineStage = {

    new PipelineStage {
      def build(context: PipelineContext, commandPL: CPL, eventPL: EPL): Pipelines = new Pipelines {

        val commandPipeline: CPL = {
          case PONG =>
            commandPL(IOPeer.Send(ByteBuffer.wrap(s"$PONG$CRLF".getBytes), None))

          case cmd =>
            commandPL(cmd)
        }

        val eventPipeline: EPL = eventPL
      }
    }
  }
}