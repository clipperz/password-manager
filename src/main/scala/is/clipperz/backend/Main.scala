package is.clipperz.backend

import is.clipperz.backend.apis.{ blobsApi, loginApi, logoutApi, staticApi, usersApi, oneTimeShareApi }
import is.clipperz.backend.functions.{ customErrorHandler }
import is.clipperz.backend.middleware.{ hashcash, metrics }
import is.clipperz.backend.services.{ BlobArchive, PRNG, SessionManager, SrpManager, TollManager, UserArchive, OneTimeShareArchive }
import is.clipperz.backend.services.ChallengeType

// import java.nio.file.FileSystems
import zio.nio.file.{ Files, FileSystem }

import scala.util.Try

import zio.{ LogLevel, Runtime, Scope, ZIOAppArgs, ZIO, ZLayer, durationInt }
import zio.logging.LogFormat
import zio.metrics.connectors.{ MetricsConfig, datadog }
import zio.http.{ HttpApp, Middleware, Server }
import zio.http.netty.{ EventLoopGroups, NettyConfig }
import zio.http.netty.NettyConfig.LeakDetectionLevel
import zio.http.Server.RequestStreaming

object Main extends zio.ZIOAppDefault:
    override val bootstrap =
        val logFormat = LogFormat.colored |-| LogFormat.spans
        Runtime.removeDefaultLoggers ++ Runtime.addLogger(CustomLogger.basicColoredLogger(LogLevel.Info)) // >>> SLF4J.slf4j(logFormat)

    type ClipperzEnvironment =
        PRNG & SessionManager & TollManager & UserArchive & BlobArchive & OneTimeShareArchive & SrpManager

    type ClipperzHttpApp = HttpApp[
        ClipperzEnvironment
    ]

    val clipperzBackend: ClipperzHttpApp = (
            usersApi        @@ hashcash(ChallengeType.REGISTER, ChallengeType.CONNECT)
        ++  loginApi        @@ hashcash(ChallengeType.CONNECT,  ChallengeType.MESSAGE)
        ++  logoutApi
        ++  blobsApi        @@ hashcash(ChallengeType.MESSAGE,  ChallengeType.MESSAGE)
        ++  oneTimeShareApi @@ hashcash(ChallengeType.SHARE,    ChallengeType.SHARE)
        ++  staticApi
    )
    .handleErrorCauseZIO(customErrorHandler)
    .toHttpApp
  
    val middlewares =
        Middleware.debug ++                                                         //  print debug info about request and response
        Middleware.timeout(20.seconds) ++                                           //  TODO: add timeout time to configuration file [fsolaroli - 10/01/2024]
        Middleware.requestLogging(logRequestBody = true, logResponseBody = true) ++ //  loggingMiddleware
        metrics()

    // val completeClipperzBackend: ClipperzHttpApp = clipperzBackend @@ (Middleware.timeout(10.seconds) ++ metrics()) //TODO: add timeout time to configuration file [fsolaroli - 10/01/2024]
    val completeClipperzBackend: ClipperzHttpApp = clipperzBackend @@ middlewares

    val keyBlobArchiveFolderDepth = 16
    // val keyBlobArchiveFolderDepth = 2

    val run: ZIO[Any & ZIOAppArgs & Scope, Any, Any] = ZIOAppArgs.getArgs.flatMap { args =>
        if args.length == 4
        then
            val blobBasePath         = FileSystem.default.getPath(args(0))
            val userBasePath         = FileSystem.default.getPath(args(1))
            val oneTimeShareBasePath = FileSystem.default.getPath(args(2))
            val port = args(3).toInt

            val MB = 1024 * 1024

            val nThreads: Int = args.headOption.flatMap(x => Try(x.toInt).toOption).getOrElse(0)

            val config        = Server.Config.default
                                    .port(port)
                                    .requestStreaming(RequestStreaming.Enabled)
                                    // .enableRequestStreaming                                    
            val nettyConfig   = NettyConfig.default
                                    .leakDetection(LeakDetectionLevel.PARANOID)
                                    .maxThreads(nThreads)

            // ( Files.createDirectories(blobBasePath) <&> 
            //   Files.createDirectories(userBasePath) <&> 
            //   Files.createDirectories(oneTimeShareBasePath)
            // ) *>
            Server
                .install(completeClipperzBackend)
                .flatMap(port =>
                    println("SERVER STARTED")
                        ZIO.logInfo(s"Server started on port ${port}")
                    *>  ZIO.never
                )
                .provide(
                    PRNG.live,
                    SessionManager.live(30.minutes), //TODO: add cache timeToLive to configuration file [fsolaroli - 10/01/2024]
                    TollManager.live,
                    UserArchive.fs(userBasePath, keyBlobArchiveFolderDepth, true),
                    BlobArchive.fs(blobBasePath, keyBlobArchiveFolderDepth, true),
                    OneTimeShareArchive.fs(oneTimeShareBasePath, keyBlobArchiveFolderDepth, true),
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
