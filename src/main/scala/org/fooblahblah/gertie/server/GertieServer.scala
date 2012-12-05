package org.fooblahblah.gertie.server

import java.util.concurrent.TimeUnit._
import akka.actor.{ActorRef, Props, ActorSystem}
import akka.pattern.ask
import spray.util._
import spray.io._
import java.util.concurrent.TimeUnit
import akka.util.Timeout
import scala.concurrent.duration.Duration


object Main extends App {
  implicit val system = ActorSystem("gertie-server")

  implicit val akkaTimeout = Timeout(Duration(1, TimeUnit.SECONDS))

  val port = 6668

  val ioBridge = IOExtension(system).ioBridge

  val server = system.actorOf(
    Props(new GertieServer(ioBridge)),
    name = "gertie-server"
  )

  server
    .ask(IOServer.Bind("localhost", port))
    .onSuccess { case IOServer.Bound(endpoint, _) =>
    println("\nBound gertie-server to " + endpoint)
    println(s"Run `telnet localhost $port`, type something and press RETURN. Type `STOP` to exit...\n")
  }
}

class GertieServer(ioBridge: ActorRef) extends IOServer(ioBridge) with ConnectionActors {

  override def pipeline = CommandParser(log) >>
    CommandHandler(log) >>
    ResponseHandler(log)

}



//  override def receive = super.receive orElse {
//    case IOBridge.Received(handle, buffer) =>
//      buffer.array.asString.trim match {
//        case "STOP" =>
//          ioBridge ! IOBridge.Send(handle, BufferBuilder("Shutting down...").toByteBuffer)
//          log.info("Shutting down")
//          context.system.shutdown()
//        case x =>
//          log.debug("Received '{}', echoing ...", x)
//          ioBridge ! IOBridge.Send(handle, buffer, Some('SentOk))
//      }
//
//    case 'SentOk =>
//      log.debug("Send completed")
//
//    case IOBridge.Closed(_, reason) =>
//      log.debug("Connection closed: {}", reason)
//  }
