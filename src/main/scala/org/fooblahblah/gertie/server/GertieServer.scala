package org.fooblahblah.gertie.server

import akka.actor.{ActorRef, Props, ActorSystem}
import akka.event.Logging
import akka.pattern.ask
import akka.util.Timeout
import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeUnit._
import scala.concurrent.duration._
import spray.util._
import spray.io._


object Main extends App {
  implicit val system = ActorSystem("gertie-server")

  implicit val akkaTimeout = Timeout(1 second)

  val logger = Logging(system, "Gertie")

  val port = 6668

  val ioBridge = IOExtension(system).ioBridge

  val server = system.actorOf(
    Props(new GertieServer(ioBridge)),
    name = "gertie-server"
  )

  server
    .ask(IOServer.Bind("localhost", port))
    .onSuccess { case IOServer.Bound(endpoint, _) =>
    logger.info(s"Bound gertie-server to ${endpoint}")
    logger.info(s"Press ctrl-c to quit...")
  }
}

class GertieServer(ioBridge: ActorRef) extends IOServer(ioBridge) with ConnectionActors {
  override def pipeline =
    CommandParser(log) >>
    CommandHandler(log) >>
    ResponseHandler(log)
}
