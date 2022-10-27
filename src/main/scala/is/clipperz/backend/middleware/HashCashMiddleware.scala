package is.clipperz.backend.middleware

import zio.ZIO
import zio.json.EncoderOps
import zhttp.http.{ Headers, Http, Middleware, Request, Response, Status }
import zhttp.http.* //TODO: fix How do you import `!!` and `/`?
import zhttp.http.middleware.HttpMiddleware
import is.clipperz.backend.data.HexString
import is.clipperz.backend.functions.{ fromString, extractPath }
import is.clipperz.backend.services.{ ChallengeType, SessionManager, TollManager, TollChallenge }
import is.clipperz.backend.Main.ClipperzHttpApp
import java.util.NoSuchElementException
import is.clipperz.backend.exceptions.BadRequestException
import zio.Cause

type TollMiddleware = HttpMiddleware[TollManager & SessionManager, Throwable]

def verifyTollNecessity(req: Request): Boolean =
  List("users", "login", "blobs").contains(extractPath(req))

def getChallengeType(req: Request): ChallengeType =
  extractPath(req) match
    case "users" => ChallengeType.REGISTER
    case "login" => ChallengeType.CONNECT
    case "blobs" => ChallengeType.MESSAGE

def getNextChallengeType(req: Request): ChallengeType =
  extractPath(req) match
    case "users" => ChallengeType.CONNECT
    case "login" => ChallengeType.MESSAGE
    case "blobs" => ChallengeType.MESSAGE

def isTollInSession(req: Request): ZIO[SessionManager, Throwable, Boolean] =
  ZIO
    .service[SessionManager]
    .zip(ZIO.attempt(req.headers.headerValue(SessionManager.sessionKeyHeaderName).get))
    .zip(ZIO.attempt(req.headers.headerValue(TollManager.tollHeader).map(HexString(_)).get))
    .zip(ZIO.attempt(req.headers.headerValue(TollManager.tollCostHeader).map(_.toInt).get))
    .flatMap((sessionManager, sessionKey, challengeToll, cost) =>
      for {
        session <- sessionManager.getSession(sessionKey)
        challengeJson <- ZIO
          .attempt(session(TollManager.tollChallengeContentKey).get)
          .mapError(e => new BadRequestException("No challenge related to this session"))
        challenge <- fromString[TollChallenge](challengeJson)
        res <- ZIO.succeed(if (challengeToll == challenge.toll && cost == challenge.cost) then true else false)
      } yield res
    )      
    .catchSome {
      case ex => ZIO.logDebugCause(s"${ex.getMessage()}", Cause.fail(ex)).as(false)
    }

def checkReceipt(req: Request): ZIO[TollManager & SessionManager, Throwable, Boolean] =
  ZIO
    .service[TollManager]
    .zip(ZIO.service[SessionManager])
    .zip(ZIO.attempt(req.headers.headerValue(TollManager.tollReceiptHeader).get))
    .zip(ZIO.attempt(req.headers.headerValue(SessionManager.sessionKeyHeaderName).get))
    .flatMap { (tollManager, sessionManager, receipt, sessionKey) =>
      for {
        session <- sessionManager.getSession(sessionKey)
        challengeJson <- ZIO
          .attempt(session(TollManager.tollChallengeContentKey).get)
          .mapError(e => new BadRequestException("No challenge related to this session"))
        challenge <- fromString[TollChallenge](challengeJson)
        res <- tollManager.verifyToll(challenge, HexString(receipt))
      } yield res
    }
    .catchSome {
      case ex => ZIO.logDebugCause(s"${ex.getMessage()}", Cause.fail(ex)).as(false)
    }

def wrongTollMiddleware(responseStatus: Status): Request => TollMiddleware = req =>
  Middleware.fromHttp(
    Http.responseZIO(
      ZIO
        .service[TollManager]
        .zip(ZIO.service[SessionManager])
        .zip(ZIO.attempt(req.headers.headerValue(SessionManager.sessionKeyHeaderName).get))
        .flatMap((tollManager, sessionManager, sessionKey) =>
          for {
            tollChallenge <- tollManager.getToll(tollManager.getChallengeCost(getChallengeType(req)))
            session <- sessionManager.getSession(sessionKey)
            _ <- sessionManager.saveSession(session + (TollManager.tollChallengeContentKey, tollChallenge.toJson))
          } yield tollChallenge
        )
        .map(tollChallenge =>
          Response(
            status = responseStatus,
            headers = Headers(
              (TollManager.tollHeader, tollChallenge.toll.toString),
              (TollManager.tollCostHeader, tollChallenge.cost.toString),
            ),
          )
        )
        .catchSome {
          case ex: NoSuchElementException =>
            ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response.status(Status.BadRequest))
        }
    )
  )

val missingTollMiddleware: Request => TollMiddleware = req => wrongTollMiddleware(Status.PaymentRequired)(req)

val correctReceiptMiddleware: Request => TollMiddleware = req =>
  Middleware.patchZIO(_ =>
    ZIO
      .service[TollManager]
      .zip(ZIO.service[SessionManager])
      .zip(ZIO.attempt(req.headers.headerValue(SessionManager.sessionKeyHeaderName).get))
      .flatMap((tollManager, sessionManager, sessionKey) =>
        for {
          tollChallenge <- tollManager.getToll(tollManager.getChallengeCost(getNextChallengeType(req)))
          session <- sessionManager.getSession(sessionKey)
          _ <- sessionManager.saveSession(session + (TollManager.tollChallengeContentKey, tollChallenge.toJson))
        } yield Patch.addHeader(
          Headers(
            (TollManager.tollHeader, tollChallenge.toll.toString),
            (TollManager.tollCostHeader, tollChallenge.cost.toString),
          )
        )
      )
      .mapError(msg =>
        println(msg)
        Some(new Exception(msg))
      ) // THIS IS SO STUPID 🤬 e 🤬
      .catchSome {
        case ex =>
          ZIO
            .logWarningCause(s"${ex.getOrElse(new Exception("An exception was launched but not?")).getMessage()}", Cause.fail(ex))
            .flatMap(_ => ZIO.fail(ex))
      }
  )

val tollPresentMiddleware: Request => TollMiddleware = req =>
  Middleware.ifThenElseZIO[Request](req => isTollInSession(req))(
    r =>
      Middleware.ifThenElseZIO[Request](r => checkReceipt(r))(
        correctReceiptMiddleware, // keep the request going and add headers with the new toll to the response
        wrongTollMiddleware(Status.PaymentRequired), //
      ),
    wrongTollMiddleware(Status.BadRequest),
  )

val hashcash: TollMiddleware = Middleware
  .ifThenElse[Request](req => req.headers.hasHeader(TollManager.tollReceiptHeader))(tollPresentMiddleware, missingTollMiddleware)
  .when(verifyTollNecessity)
