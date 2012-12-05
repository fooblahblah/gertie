package org.fooblahblah.gertie.server

import akka.event.LoggingAdapter
import spray.io._
import spray.util._
import org.fooblahblah.gertie.parser._
import IRCCommands.{Command => IRCCommand}
import java.nio.ByteBuffer

object CommandHandler {
  import IRCCommands._
  import Commands._

  def apply(log: LoggingAdapter): PipelineStage = {

    new PipelineStage {
      def build(context: PipelineContext, commandPL: CPL, eventPL: EPL): Pipelines = new Pipelines {

        val commandPipeline: CPL = {
          case WrappedCommand(cmd) => cmd match {
            case PING =>
              commandPL(PONG)

            case cmd  =>
              log.warning(s"Unhandled command: $cmd")
          }

          case cmd =>
            commandPL(cmd)
        }

        val eventPipeline: EPL = eventPL
      }
    }
  }
}