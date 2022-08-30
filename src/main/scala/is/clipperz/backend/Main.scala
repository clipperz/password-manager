package is.clipperz.backend

import java.io.FileInputStream
import java.io.File
import java.nio.file.FileSystems
import scala.util.Try
import io.netty.handler.ssl.util.SelfSignedCertificate
import zio.{ Task, ZIO, Scope, ZIOAppArgs, ZIOAppDefault }
import zio.stream.{ ZSink, ZStream }
import zio.json.{ DecoderOps, EncoderOps }
import zhttp.http.{ Headers, HeaderNames, HeaderValues, Http, HttpApp, HttpData, Method, Middleware, Path, Request, Response, Status }
import zhttp.http.* //TODO: fix How do you import `!!` and `/`?
import zhttp.http.middleware.{ HttpMiddleware }
import zhttp.service.{ EventLoopGroup, Server }
import zhttp.service.server.ServerChannelFactory
import is.clipperz.backend.services.TollManager
import is.clipperz.backend.data.HexString
import is.clipperz.backend.functions.fromStream
import is.clipperz.backend.functions.SrpFunctions.*
import is.clipperz.backend.services.{
  BlobArchive,
  ChallengeType,
  PRNG,
  SaveBlobData, 
  SessionManager,
  SignupData,
  SrpManager,
  SRPStep1Data,
  SRPStep2Data,
  TollManager,
  UserArchive,
  UserCard
}
import zhttp.http.PathSyntax

object Main extends zio.ZIOAppDefault:
  private val PORT = 8090

  type ClipperzEnvironment =
    PRNG & SessionManager & TollManager & UserArchive & BlobArchive & SrpManager

  type ClipperzHttpApp = HttpApp[
    ClipperzEnvironment,
    Throwable,
  ]

  type TollMiddleware = HttpMiddleware[TollManager, Throwable]

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  val static = Http.collectHttp[Request] { 
    case request @ Method.GET -> !! / file =>
      Http.fromFile(File(s"target/output.parcel/${file}"))
    }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  val users: ClipperzHttpApp = Http.collectZIO {
    case request @ Method.PUT -> !! / "users" / c =>
      ZIO.service[UserArchive]
        .zip(ZIO.service[BlobArchive])
        .zip(ZIO.service[SessionManager])
        .zip(ZIO.succeed(request.bodyAsStream))
        .flatMap((userArchive, blobArchive, sessionManager, content) =>
          userArchive.getUser(HexString(c)).flatMap(optionalUser => optionalUser match
            case Some(_) => sessionManager.verifySessionUser(c, request)
            case None    => ZIO.succeed(())
          ).flatMap(_ =>
            fromStream[SignupData](content)
            .flatMap(signupData => {
              if HexString(c) == signupData.user.c then
                ( userArchive.saveUser(signupData.user, false)
                  <&> //Returns an effect that executes both this effect and the specified effect, in parallel, combining their results into a tuple. If either side fails, then the other side will be interrupted.
                  blobArchive.saveBlob(signupData.indexCardReference, ZStream.fromIterable(signupData.indexCardContent.toByteArray))
                  <&>
                  ZIO.foreach(signupData.cards) { (reference, content) => blobArchive.saveBlob(reference, ZStream.fromIterable(content.toByteArray)) }
                ).parallelErrors.foldZIO(
                    _      => ZIO.fail(new Exception("TODO")),
                    result => ZIO.succeed(result)
                )                
              else  
                ZIO.fail(new Exception("c in request path differs from c in request body "))
            })
          )
        ).fold(
          err => { println(s"ERROR ${err}"); Response(status = Status.Conflict) },
          results => Response.text(results._1.toString)
        )

    case request @ Method.DELETE -> !! / "users" / c =>
      ZIO.service[UserArchive]
        .zip(ZIO.service[SessionManager])
        .flatMap((userArchive, sessionManager) =>
          sessionManager.verifySessionUser(c, request)
          .flatMap(_ => userArchive.deleteUser(HexString(c)))
        )
        .map(_ => Response.text(c))
  }
  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  val login: ClipperzHttpApp = Http.collectZIO {
    case request @ Method.POST -> !! / "login" / "step1" / c =>
      ZIO.service[SessionManager]
        .zip(ZIO.service[SrpManager])
        .zip(ZIO.attempt(request.headers.headerValue(SessionManager.sessionKeyHeaderName).get))
        .zip(ZIO.succeed(request.bodyAsStream))
        .flatMap((sessionManager, srpManager, sessionKey, content) =>
          fromStream[SRPStep1Data](content)
          .flatMap(loginStep1Data => {
            if HexString(c) == loginStep1Data.c then
              for {
                session <- sessionManager.getSession(sessionKey) //create new session
                (step1Response, session) <- srpManager.srpStep1(loginStep1Data, session)
                _ <- sessionManager.saveSession(session)
              } yield step1Response
            else  
              ZIO.fail(new Exception("c in request path differs from c in request body "))
          })
        )
        .either
        .map(e => e.fold(err => { println(s"LOGIN STEP 1: ERROR ${err}"); Response(status = Status.InternalServerError) }, (step1Response) => Response.json(step1Response.toJson)))

    case request @ Method.POST -> !! / "login" / "step2" / c => 
      ZIO.service[SessionManager]
        .zip(ZIO.service[SrpManager])
        .zip(ZIO.attempt(request.headers.headerValue(SessionManager.sessionKeyHeaderName).get))
        .zip(ZIO.succeed(request.bodyAsStream))
        .flatMap((sessionManager, srpManager, sessionKey, content) =>
          fromStream[SRPStep2Data](content)
          .flatMap(loginStep2Data => {
              for {
                session <- sessionManager.getSession(sessionKey)
                // _ <- ZIO.succeed(println(s"OPTIONAL SESSION: ${optionalSession}"))
                (step2Response, session) <- srpManager.srpStep2(loginStep2Data, session)
                _ <- sessionManager.saveSession(session)
              } yield step2Response
          })
        )
        .either
        .map(e => e.fold(err => { println(s"LOGIN STEP 2: ERROR ${err}"); Response(status = Status.InternalServerError) }, (step2Response) => Response.json(step2Response.toJson)))

  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  val blobs: ClipperzHttpApp = Http.collectZIO {
    case request @ Method.POST -> !! / "blobs" =>
      ZIO
        .service[BlobArchive]
        .zip(ZIO.succeed(request.bodyAsStream))
        .flatMap((archive, bytes) => 
          fromStream[SaveBlobData](bytes)
          .flatMap(saveData =>
            archive.saveBlob(saveData.hash, ZStream.fromIterable(saveData.data.toByteArray))
          )
        )
        .map(hash => Response.text(s"${hash}"))

    case request @ Method.DELETE -> !! / "blobs" / hash =>
      ZIO
        .service[BlobArchive]
        .zip(ZIO.succeed(request.bodyAsStream))
        .flatMap((archive, blob) => archive.deleteBlob(blob))
        .map(_ => Response.ok)

    case request @ Method.GET -> !! / "blobs" / hash =>
      ZIO
        .service[BlobArchive]
        .flatMap(archive => archive.getBlob(HexString(hash)))
        .map((bytes: ZStream[Any, Throwable, Byte]) =>
          Response(
            status = Status.Ok,
            data = HttpData.fromStream(bytes),
            headers = Headers(HeaderNames.contentType, HeaderValues.applicationOctetStream),
          )
        )
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  val clipperzBackend: ClipperzHttpApp = { users ++ login ++ blobs ++ static}

  def extractPath(req: Request): String = 
    if req.path.leadingSlash 
      then req.path.dropTrailingSlash.drop(1).take(1).toString 
      else req.path.dropTrailingSlash.take(1).toString

  def verifyTollNecessity(req: Request): Boolean =
    List("users", "login", "blobs").contains(extractPath(req))

  def getChallengeType(req: Request): ChallengeType = 
    extractPath(req) match
      case "users" => ChallengeType.REGISTER
      case "login" => ChallengeType.CONNECT
      case "blobs" => ChallengeType.MESSAGE

  val missingTollMiddleware: Request => TollMiddleware = req =>
    Middleware.fromHttp(
        Http.responseZIO (
          ZIO
            .service[TollManager]
            .flatMap(tollManager => tollManager.getToll(tollManager.getChallengeCost(getChallengeType(req))))
            .map(tollChallenge => 
              Response(
                status = Status.BadRequest,
                headers = Headers((TollManager.tollHeader, tollChallenge.toll.toString), 
                                  (TollManager.tollCostHeader, tollChallenge.cost.toString))
              )
            )
        )
      ).when(verifyTollNecessity)

  val tollPresentMiddleware: Request => TollMiddleware = req => Middleware.fromHttp(Http.status(Status.Ok).addHeaders(Headers()))

  val hashcash: TollMiddleware = Middleware.ifThenElse[Request]
    (req => req.headers.hasHeader(TollManager.tollReceiptHeader) && req.headers.hasHeader(TollManager.tollCostHeader))
      ( tollPresentMiddleware
      , missingTollMiddleware
      )

  // -------------------------------------------------------------------------

  val server =
    Server.port(PORT) ++
    Server.paranoidLeakDetection ++
    Server.app(clipperzBackend/*  @@ hashcash */)

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
