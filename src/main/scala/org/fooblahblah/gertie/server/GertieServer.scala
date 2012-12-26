package org.fooblahblah.gertie.server

import akka.actor.{ActorRef, Props, ActorSystem}
import akka.event.Logging
import akka.pattern.ask
import akka.util.Timeout
import com.typesafe.config._
import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeUnit._
import scala.concurrent.duration._
import spray.io._
import spray.util._
import spray.io.IOBridge.Settings


object Main extends App {
  implicit val system      = ActorSystem("gertie-server")
  implicit val akkaTimeout = Timeout(1 second)

  val config = ConfigFactory.load()

  val logger = Logging(system, "Gertie")

  val iface = config.getString("gertie.interface")
  val port  = config.getInt("gertie.port")

  val ioBridge = IOExtension(system).ioBridge(new Settings(config))

  val server = system.actorOf(
    Props(new GertieServer(ioBridge)),
    name = "gertie-server"
  )

  server
    .ask(IOServer.Bind(iface, port))
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
