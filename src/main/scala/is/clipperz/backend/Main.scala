package is.clipperz.backend

import java.nio.file.FileSystems
import scala.util.Try
import zio.{ ZIO, Scope, ZIOAppArgs, ZIOAppDefault, LogLevel, Runtime, ZLayer }
import zio.durationInt
import zio.logging.LogFormat
import zio.logging.backend.SLF4J
import zio.metrics.connectors.datadog
import zio.metrics.connectors.MetricsConfig

import zio.http.{
  Header,
  Headers,
  Http,
  HttpApp,
  Method,
  Path,
  PathSyntax,
  Request,
  Response,
  Status,
}
import zio.http.{ Server }
import zio.http.netty.{ EventLoopGroups, NettyConfig }
import zio.http.netty.NettyConfig.LeakDetectionLevel

import is.clipperz.backend.apis.{ blobsApi, loginApi, logoutApi, staticApi, usersApi, oneTimeShareApi }
import is.clipperz.backend.middleware.{ hashcash, sessionChecks }
import is.clipperz.backend.services.{ BlobArchive, PRNG, SessionManager, SrpManager, TollManager, UserArchive, OneTimeShareArchive }

object Main extends zio.ZIOAppDefault:
  override val bootstrap =
    val logFormat = LogFormat.colored |-| LogFormat.spans
    Runtime.removeDefaultLoggers ++ Runtime.addLogger(CustomLogger.basicColoredLogger(LogLevel.Info)) // >>> SLF4J.slf4j(logFormat)
    // Runtime.removeDefaultLoggers ++ Runtime.addLogger(CustomLogger.basicLogger) // >>> SLF4J.slf4j(logFormat)

  type ClipperzEnvironment =
    PRNG & SessionManager & TollManager & UserArchive & BlobArchive & OneTimeShareArchive & SrpManager

  type ClipperzHttpApp = HttpApp[
    ClipperzEnvironment,
    Throwable,
  ]

  val clipperzBackend: ClipperzHttpApp = usersApi ++ loginApi ++ logoutApi ++ blobsApi ++ oneTimeShareApi ++ staticApi
  val completeClipperzBackend: ClipperzHttpApp = clipperzBackend @@ (sessionChecks ++ hashcash)

  val run = ZIOAppArgs.getArgs.flatMap { args =>
    if args.length == 4 then
      val blobBasePath = FileSystems.getDefault().nn.getPath(args(0)).nn
      val userBasePath = FileSystems.getDefault().nn.getPath(args(1)).nn
      val oneTimeShareBasePath = FileSystems.getDefault().nn.getPath(args(2)).nn
      val port = args(3).toInt

      val MB = 1024 * 1024

      val nThreads: Int = args.headOption.flatMap(x => Try(x.toInt).toOption).getOrElse(0)

      val config           = Server.Config.default
                              .port(port)
                              // .withRequestStreaming(RequestStreaming.Enabled) //TODO: enable and change handling in frontend
      val nettyConfig      = NettyConfig.default
                              .leakDetection(LeakDetectionLevel.PARANOID)
                              .maxThreads(nThreads)

      blobBasePath.toFile().nn.mkdirs()
      userBasePath.toFile().nn.mkdirs()
      oneTimeShareBasePath.toFile().nn.mkdirs()

      Server
        .install(completeClipperzBackend.withDefaultErrorResponse)
        .flatMap(port =>
             ZIO.logInfo(s"Server started on port ${port}")
          *> ZIO.never
        )
        .provide(
          PRNG.live,
          SessionManager.live,
          TollManager.live,
          UserArchive.fs(userBasePath, 2, true),
          BlobArchive.fs(blobBasePath, 2, true),
          OneTimeShareArchive.fs(oneTimeShareBasePath, 2, true),
          SrpManager.v6a(),
          
          ZLayer.succeed(config),
          ZLayer.succeed(nettyConfig),
          Server.customized,
          
          datadog.datadogLayer,
          ZLayer.succeed(datadog.DatadogConfig("dd-agent", 8125)),
          ZLayer.succeed(MetricsConfig(100.millis)),
        )

    else ZIO.logFatal("Not enough arguments")
  }
