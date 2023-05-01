package is.clipperz.backend

import java.nio.file.FileSystems
import scala.util.Try
import zio.{ ZIO, Scope, ZIOAppArgs, ZIOAppDefault }
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
import zio.http.netty.{ EventLoopGroups }
// import zio.http.service.server.ServerChannelFactory
import is.clipperz.backend.apis.{ blobsApi, loginApi, logoutApi, staticApi, usersApi, oneTimeShareApi }
import is.clipperz.backend.middleware.{ hashcash, sessionChecks }
import is.clipperz.backend.services.{ BlobArchive, PRNG, SessionManager, SrpManager, TollManager, UserArchive }

import zio.logging.LogFormat
import zio.logging.backend.SLF4J
import zio.{ LogLevel, Runtime }
import zio.ZLogger
import zio.Trace
import zio.FiberId
import zio.Cause
import zio.FiberRefs
import zio.LogSpan
import zio.http.HttpError.Custom
import is.clipperz.backend.services.OneTimeShareArchive
import zio.ZLayer
import zio.http.netty.NettyConfig
import zio.http.netty.NettyConfig.LeakDetectionLevel
import zio.http.Server.RequestStreaming

object Main extends zio.ZIOAppDefault:
  override val bootstrap =
    val logFormat = LogFormat.colored |-| LogFormat.spans
    Runtime.removeDefaultLoggers ++ Runtime.addLogger(CustomLogger.coloredLogger(LogLevel.Info)) // >>> SLF4J.slf4j(logFormat)

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
      val blobsArchivePath = args(0).split("/").nn
      val usersArchivePath = args(1).split("/").nn
      val oneTimeShareArchivePath = args(2).split("/").nn
      val port = args(3).toInt

      val MB = 1024 * 1024

      val nThreads: Int = args.headOption.flatMap(x => Try(x.toInt).toOption).getOrElse(0)

      val config           = Server.Config.default
                              .port(port)
                              // .withRequestStreaming(RequestStreaming.Enabled) //TODO: enable and change handling in frontend
      val nettyConfig      = NettyConfig.default
                              .leakDetection(LeakDetectionLevel.PARANOID)
                              .maxThreads(nThreads)

      val blobBasePath = FileSystems.getDefault().nn.getPath(blobsArchivePath(0), blobsArchivePath.drop(1)*).nn
      val userBasePath = FileSystems.getDefault().nn.getPath(usersArchivePath(0), usersArchivePath.drop(1)*).nn
      val oneTimeShareBasePath = FileSystems.getDefault().nn.getPath(oneTimeShareArchivePath(0), oneTimeShareArchivePath.drop(1)*).nn

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
          ZLayer.succeed(config), ZLayer.succeed(nettyConfig), Server.customized
        )

    else ZIO.logFatal("Not enough arguments")
  }
