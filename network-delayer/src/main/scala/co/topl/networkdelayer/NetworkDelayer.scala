package co.topl.networkdelayer

import cats.data.OptionT
import cats.effect.kernel.Resource
import cats.effect.{Async, IO}
import cats.implicits._
import cats.effect.implicits._
import co.topl.common.application.IOBaseApp
import com.comcast.ip4s._
import com.typesafe.config.Config
import fs2._
import fs2.io.net.{Network, Socket}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration._

/**
 * An application  which intercepts TCP connections and induces bandwidth and latency throttling.
 */
object NetworkDelayer
    extends IOBaseApp[Args, ApplicationConfig](
      createArgs = args => IO.delay(Args.parserArgs.constructOrThrow(args)),
      createConfig = IOBaseApp.createTypesafeConfig,
      parseConfig = (_: Args, conf) => IO.delay(ApplicationConfig.unsafe(conf))
    ) {

  implicit private val logger: Logger[F] =
    Slf4jLogger.getLoggerFromClass[F](this.getClass)

  def run(args: Args, config: Config, appConfig: ApplicationConfig): IO[Unit] =
    for {
      _ <- Logger[F].info("Launching NetworkDelayer")
      _ <- Logger[F].info(show"args=$args")
      _ <- Logger[F].info(show"config=$appConfig")
      _ <- appConfig.routes.parTraverse(serverForRoute).void
    } yield ()

  /**
   * Handle the given route configuration by binding the local host/port and forwarding any incoming request payloads
   * to the route configuration's destination.  The configuration's throttle is also applied to the connection.
   */
  private def serverForRoute(route: ApplicationConfig.Route): F[Unit] =
    (
      for {
        serverStream <- buildServerStream(route)
        _ <- Logger[F].info(s"Serving at binding=${route.bindHost}:${route.bindPort} with throttle=${route.throttle}")
        _ <- serverStream
          .map(handleSocket(route)(buildClientResource(route))(_))
          .parJoinUnbounded
          .compile
          .drain
      } yield ()
    ).handleErrorWith(Logger[F].error(_)(s"Connection failed at binding=${route.bindHost}:${route.bindPort}"))

  /**
   * Validate the route settings for the local binding, and bind the port to return a stream of inbound connections
   */
  private def buildServerStream(route: ApplicationConfig.Route): F[Stream[F, Socket[F]]] =
    for {
      bindHost <- OptionT
        .fromOption[F](Host.fromString(route.bindHost))
        .getOrRaise(new IllegalArgumentException("Invalid bindHost"))
      bindPort <- OptionT
        .fromOption[F](Port.fromInt(route.bindPort))
        .getOrRaise(new IllegalArgumentException("Invalid bindPort"))
    } yield Network[F].server(bindHost.some, bindPort.some)

  /**
   * Validate the route's destination and construct a reusable destination connection resource.
   */
  private def buildClientResource(route: ApplicationConfig.Route): Resource[F, Socket[F]] =
    for {
      destinationHost <- OptionT
        .fromOption[F](Host.fromString(route.destinationHost))
        .getOrRaise(new IllegalArgumentException("Invalid destinationHost"))
        .toResource
      destinationPort <- OptionT
        .fromOption[F](Port.fromInt(route.destinationPort))
        .getOrRaise(new IllegalArgumentException("Invalid destinationPort"))
        .toResource
      client <- Network[F].client(SocketAddress(destinationHost, destinationPort))
    } yield client

  /**
   * Forward inbound data from the local socket to the destination.  Forward inbound data from the destination
   * to the local socket.  Apply throttling if configured.
   */
  private def handleSocket(route: ApplicationConfig.Route)(clientResource: Resource[F, Socket[F]])(
    localSocket: Socket[F]
  ): Stream[F, Unit] =
    Stream
      .resource(clientResource)
      .evalTap(_ =>
        Logger[F].info(s"Accepted inbound connection at binding=${route.bindHost}:${route.bindPort}") >>
        Logger[F].info(
          s"Forwarding from binding=${route.bindHost}:${route.bindPort}" +
          s" to remote=${route.destinationHost}:${route.destinationPort}"
        )
      )
      .flatMap(clientSocket =>
        download(route)(clientSocket.reads)(localSocket.writes)
          .merge(upload(route)(localSocket.reads)(clientSocket.writes))
      )
      .void
      .handleErrorWith(e => Stream.exec(Logger[F].error(e)("Connection failed")))

  /**
   * Handle the "download" side of the socket, and impose throttling
   */
  private def download(route: ApplicationConfig.Route)(in: Stream[F, Byte])(out: Pipe[F, Byte, Nothing]) =
    route.throttle
      .foldLeft(in)(_ through downloadThrottler(_))
      .through(out)

  /**
   * Handle the "upload" side of the socket, and impose throttling
   */
  private def upload(route: ApplicationConfig.Route)(in: Stream[F, Byte])(out: Pipe[F, Byte, Nothing]) =
    route.throttle
      .foldLeft(in)(_ through uploadThrottler(_))
      .through(out)

  private def downloadThrottler(throttle: ApplicationConfig.Route.Throttle): Pipe[F, Byte, Byte] =
    _.through(latencyThrottler(throttle.latency)).through(bandwidthThrottler(throttle.downloadBytesPerSecond))

  private def uploadThrottler(throttle: ApplicationConfig.Route.Throttle): Pipe[F, Byte, Byte] =
    _.through(latencyThrottler(throttle.latency)).through(bandwidthThrottler(throttle.uploadBytesPerSecond))

  private def bandwidthThrottler(bytesPerSecond: Long): Pipe[F, Byte, Byte] =
    _.metered(1.seconds / bytesPerSecond)

  private def latencyThrottler(latency: FiniteDuration): Pipe[F, Byte, Byte] =
    _.chunks.parEvalMapUnbounded(v => Async[F].delayBy(Async[F].pure(v), latency)).unchunks

}
