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
  HttpData,
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

object Main extends zio.ZIOAppDefault:
  private val PORT = 8090

  type ClipperzEnvironment =
    PRNG & SessionManager & TollManager & UserArchive & BlobArchive & SrpManager

  type ClipperzHttpApp = HttpApp[
    ClipperzEnvironment,
    Throwable,
  ]

  val clipperzBackend: ClipperzHttpApp = { usersApi ++ loginApi ++ logoutApi ++ blobsApi ++ staticApi }
  val completeClipperzBackend: ClipperzHttpApp = clipperzBackend @@ (sessionChecks ++ hashcash)

  val server =
    Server.port(PORT) ++
      Server.paranoidLeakDetection ++
      Server.app(completeClipperzBackend)

  val run = ZIOAppArgs.getArgs.flatMap { args =>
    val nThreads: Int = args.headOption.flatMap(x => Try(x.toInt).toOption).getOrElse(0)

    val blobBasePath = FileSystems.getDefault().nn.getPath("target", "archive", "blobs").nn
    val userBasePath = FileSystems.getDefault().nn.getPath("target", "archive", "users").nn

    server
      .make
      .flatMap(start => zio.Console.printLine(s"Server started on port ${start.port}") *> ZIO.never)
      .provide(
        PRNG.live,
        SessionManager.live,
        TollManager.live,
        UserArchive.fs(userBasePath, 2),
        BlobArchive.fs(blobBasePath, 2),
        SrpManager.v6a(),
        ServerChannelFactory.auto,
        EventLoopGroup.auto(nThreads),
        Scope.default,
      )
  }
