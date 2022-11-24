package is.clipperz.backend

import java.nio.file.FileSystems
import scala.util.Try
import zio.{ ZIO, Scope, ZIOAppArgs, ZIOAppDefault }
import zhttp.http.{
  Header,
  Headers,
  HeaderNames,
  HeaderValues,
  Http,
  HttpApp,
  Method,
  Middleware,
  Path,
  PathSyntax,
  Request,
  Response,
  Status,
}
import zhttp.service.{ EventLoopGroup, Server }
import zhttp.service.server.ServerChannelFactory
import is.clipperz.backend.apis.{ blobsApi, loginApi, logoutApi, staticApi, usersApi }
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
import zhttp.http.HttpError.Custom

object Main extends zio.ZIOAppDefault:
  override val bootstrap =
    val logFormat = LogFormat.colored |-| LogFormat.spans
    Runtime.removeDefaultLoggers ++ Runtime.addLogger(CustomLogger.coloredLogger(LogLevel.Info)) // >>> SLF4J.slf4j(logFormat)

  type ClipperzEnvironment =
    PRNG & SessionManager & TollManager & UserArchive & BlobArchive & SrpManager

  type ClipperzHttpApp = HttpApp[
    ClipperzEnvironment,
    Throwable,
  ]

  val clipperzBackend: ClipperzHttpApp = { usersApi ++ loginApi ++ logoutApi ++ blobsApi ++ staticApi }
  val completeClipperzBackend: ClipperzHttpApp = clipperzBackend @@ (sessionChecks ++ hashcash)

  val run = ZIOAppArgs.getArgs.flatMap { args =>
    if args.length == 3 then
      val blobsArchivePath = args(0).split("/").nn
      val usersArchivePath = args(1).split("/").nn
      val port = args(2).toInt

      val server =
        Server.port(port) ++
          Server.enableObjectAggregator() ++
          Server.paranoidLeakDetection ++
          Server.app(completeClipperzBackend)

      val nThreads: Int = args.headOption.flatMap(x => Try(x.toInt).toOption).getOrElse(0)

      val blobBasePath = FileSystems.getDefault().nn.getPath(blobsArchivePath(0), blobsArchivePath.drop(1)*).nn
      val userBasePath = FileSystems.getDefault().nn.getPath(usersArchivePath(0), usersArchivePath.drop(1)*).nn

      blobBasePath.toFile().nn.mkdirs()
      userBasePath.toFile().nn.mkdirs()

      server
        .make
        // .flatMap(start => zio.Console.printLine(s"Server started on port ${start.port}") *> ZIO.never)
        .flatMap(start => ZIO.logInfo(s"Server started on port ${start.port}") *> ZIO.never)
        .provide(
          PRNG.live,
          SessionManager.live,
          TollManager.live,
          UserArchive.fs(userBasePath, 2, true),
          BlobArchive.fs(blobBasePath, 2, true),
          SrpManager.v6a(),
          ServerChannelFactory.auto,
          EventLoopGroup.auto(nThreads),
          Scope.default,
        )
    else ZIO.logFatal("Not enough arguments")
  }
