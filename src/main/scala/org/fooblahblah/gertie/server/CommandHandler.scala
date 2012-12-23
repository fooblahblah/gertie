package org.fooblahblah.gertie.server

import akka.actor._
import akka.event.LoggingAdapter
import akka.pattern._
import akka.util.Timeout
import java.nio.ByteBuffer
import org.fooblahblah.bivouac.Bivouac
import org.fooblahblah.bivouac.model.Model._
import org.fooblahblah.gertie.parser._
import org.fooblahblah.gertie.util.Utils._
import scala.annotation._
import scala.concurrent._
import scala.concurrent.duration._
import spray.io._
import spray.io.IOBridge.Closed
import spray.util._

object CommandHandler {
  import IRCCommands._
  import Commands._

  def apply(log: LoggingAdapter): PipelineStage = {
    new PipelineStage {
      def build(context: PipelineContext, commandPL: CPL, eventPL: EPL): Pipelines = new Pipelines {

        implicit val system  = context.connectionActorContext.system
        implicit val timeout = Timeout(5 seconds)

        val ircActorRef = system.actorOf(Props(new IRCConnectionProtocol(context, commandPL)))

        val commandPipeline: CPL = {

          case WrappedCommand(cmd) =>
            ircActorRef ! cmd

          case cmd =>
            commandPL(cmd)
        }


        val eventPipeline: EPL = {
          case e: Closed =>
            (ircActorRef ? CHANNEL_CACHE).mapTo[(Map[String, Room], Map[String, ActorRef])] map { t =>
              val (channelCache, joinedChannels) = t

              joinedChannels.foreach { pair =>
                val (name, ref) = pair

                channelCache.get(name) map { room =>
                  log.info(s"Leaving $name")
                  ref ! PoisonPill
                  ircActorRef ! PART(Seq(name))
                }
              }
            }
            eventPL(e)


          case e =>
            eventPL(e)
        }
     }
    }
  }
}