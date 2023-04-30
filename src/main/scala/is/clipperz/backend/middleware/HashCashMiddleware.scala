package is.clipperz.backend.middleware

import zio.ZIO
import zio.json.EncoderOps
import zio.http.{ Headers, Http, Request, Response, Status }
import zio.http.* //TODO: fix How do you import `!!` and `/`?
import is.clipperz.backend.data.HexString
import is.clipperz.backend.functions.{ fromString, extractPath }
import is.clipperz.backend.services.{ ChallengeType, SessionManager, TollManager, TollChallenge }
import is.clipperz.backend.Main.ClipperzHttpApp
import java.util.NoSuchElementException
import is.clipperz.backend.exceptions.BadRequestException
import zio.Cause
import is.clipperz.backend.LogAspect
import zio.http.Handler.RequestHandlerSyntax
import zio.http.RequestHandlerMiddleware.Simple
import zio.Trace

type TollMiddleware = RequestHandlerMiddleware[Nothing, TollManager & SessionManager, Throwable, Any]

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
    .zip(ZIO.attempt(req.rawHeader(SessionManager.sessionKeyHeaderName).get))
    .zip(ZIO.attempt(req.rawHeader(TollManager.tollHeader).map(HexString(_)).get))
    .zip(ZIO.attempt(req.rawHeader(TollManager.tollCostHeader).map(_.toInt).get))
    .flatMap((sessionManager, sessionKey, challengeToll, cost) =>
      for {
        session <- sessionManager.getSession(sessionKey)
        challengeJson <- ZIO
          .attempt(session(TollManager.tollChallengeContentKey).get)
          .mapError(e => new BadRequestException("No challenge related to this session"))
        challenge <- fromString[TollChallenge](challengeJson)
        res <- ZIO.succeed(if challengeToll == challenge.toll && cost == challenge.cost then true else false)
      } yield res
    )
    .catchSome {
      case ex => ZIO.logDebugCause(s"${ex.getMessage()}", Cause.fail(ex)).as(false)
    } @@ LogAspect.logAnnotateRequestData(req)

def checkReceipt(req: Request): ZIO[TollManager & SessionManager, Throwable, Boolean] =
  ZIO
    .service[TollManager]
    .zip(ZIO.service[SessionManager])
    .zip(ZIO.attempt(req.rawHeader(TollManager.tollReceiptHeader).get))
    .zip(ZIO.attempt(req.rawHeader(SessionManager.sessionKeyHeaderName).get))
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
      case ex => ZIO.logInfoCause(s"${ex.getMessage()}", Cause.fail(ex)).as(false)
    } @@ LogAspect.logAnnotateRequestData(req)

def wrongTollMiddleware(responseStatus: Status)(req: Request) = new RequestHandlerMiddleware.Simple[SessionManager & TollManager, Throwable]:
  override def apply[Env <: SessionManager & TollManager, Err >: Throwable](handler: Handler[Env, Err, Request, Response])(implicit trace: Trace): Handler[Env, Err, Request, Response] = 
    Handler.responseZIO(
      ZIO
        .service[TollManager]
        .zip(ZIO.service[SessionManager])
        .zip(ZIO.attempt(req.rawHeader(SessionManager.sessionKeyHeaderName).get))
        .flatMap((tollManager, sessionManager, sessionKey) =>
          for {
            _ <- ZIO.logInfo(s"Wrong toll, answering with new challenge and ${responseStatus}")
            tollChallenge <- tollManager.getToll(tollManager.getChallengeCost(getChallengeType(req)))
            session <- sessionManager.getSession(sessionKey)
            _ <- sessionManager.saveSession(session + (TollManager.tollChallengeContentKey, tollChallenge.toJson))
          } yield tollChallenge
        )
        .map(tollChallenge =>
          Response()
            .withStatus(responseStatus)
            .addHeader(TollManager.tollHeader, tollChallenge.toll.toString)
            .addHeader(TollManager.tollCostHeader, tollChallenge.cost.toString)
        )
        .catchSome {
          case ex: NoSuchElementException =>
            ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response.status(Status.BadRequest))
          case ex =>
            ZIO.logErrorCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response.status(Status.InternalServerError))
        } @@ LogAspect.logAnnotateRequestData(req)
      )

val missingTollMiddleware: Request => TollMiddleware = req => wrongTollMiddleware(Status.PaymentRequired)(req)

val correctReceiptMiddleware: Request => TollMiddleware = req =>
  RequestHandlerMiddlewares.updateResponseZIO(res =>
    ZIO
      .service[TollManager]
      .zip(ZIO.service[SessionManager])
      .zip(ZIO.attempt(req.rawHeader(SessionManager.sessionKeyHeaderName).get))
      .flatMap((tollManager, sessionManager, sessionKey) =>
        for {
          tollChallenge <- tollManager.getToll(tollManager.getChallengeCost(getNextChallengeType(req)))
          session <- sessionManager.getSession(sessionKey)
          _ <- sessionManager.saveSession(session + (TollManager.tollChallengeContentKey, tollChallenge.toJson))
        } yield
            res
              .addHeader(TollManager.tollHeader, tollChallenge.toll.toString)
              .addHeader(TollManager.tollCostHeader, tollChallenge.cost.toString)
      )
      .catchSome {
        case ex =>
          ZIO
            .logWarningCause(s"${ex.getMessage()}", Cause.fail(ex))
            .flatMap(_ => ZIO.fail(ex))
      }
  )

val tollPresentMiddleware: Request => TollMiddleware = req =>
  RequestHandlerMiddlewares.ifRequestThenElseZIO[TollManager & SessionManager, Throwable](req => for {
    tollPresent <- isTollInSession(req)
    receiptOk <- checkReceipt(req)
  } yield (tollPresent && receiptOk)) (
    correctReceiptMiddleware(req), // keep the request going and add headers with the new toll to the response
    wrongTollMiddleware(Status.PaymentRequired)(req)
  )

val hashcash: TollMiddleware = RequestHandlerMiddlewares
  .ifRequestThenElseFunction[TollManager & SessionManager, Throwable](req => req.hasHeader(TollManager.tollReceiptHeader))(tollPresentMiddleware, missingTollMiddleware)
  .when(verifyTollNecessity)

//===============//===============//===============//===============//===============//===============//===============

// val hashcash: TollMiddleware = new RequestHandlerMiddleware.Simple[SessionManager & TollManager, Throwable] {
//   override def apply[Env <: SessionManager & TollManager, Err >: Throwable](handler: Handler[Env, Err, Request, Response])(implicit trace: Trace): Handler[Env, Err, Request, Response] = 
//     Handler.fromFunctionZIO[Request]( req => 
//       ZIO.ifZIO(ZIO.succeed(req.hasHeader(TollManager.tollReceiptHeader)))(
//         ZIO.ifZIO(
//           for {
//             tollPresent <- isTollInSession(req)
//             receiptOk <- checkReceipt(req)
//           } yield (tollPresent && receiptOk)
//         )(
//           ZIO.succeed(correctReceiptHandler(req)),
//           ZIO.succeed(wrongTollHandler(Status.PaymentRequired))
//         ),
//         ZIO.succeed(wrongTollHandler(Status.PaymentRequired))
//       )
//     ).flatten
// }

// def wrongTollHandler(responseStatus: Status) = Handler.fromFunctionZIO[Request] (req => 
//   ZIO
//     .service[TollManager]
//     .zip(ZIO.service[SessionManager])
//     .zip(ZIO.attempt(req.rawHeader(SessionManager.sessionKeyHeaderName).get))
//     .flatMap((tollManager, sessionManager, sessionKey) =>
//       for {
//         _ <- ZIO.logInfo(s"Wrong toll, answering with new challenge and ${responseStatus}")
//         tollChallenge <- tollManager.getToll(tollManager.getChallengeCost(getChallengeType(req)))
//         session <- sessionManager.getSession(sessionKey)
//         _ <- sessionManager.saveSession(session + (TollManager.tollChallengeContentKey, tollChallenge.toJson))
//       } yield tollChallenge
//     )
//     .map(tollChallenge =>
//       Handler
//         .status(responseStatus)
//         .addHeader(TollManager.tollHeader, tollChallenge.toll.toString)
//         .addHeader(TollManager.tollCostHeader, tollChallenge.cost.toString)
//     )
//     .catchSome {
//       case ex: NoSuchElementException =>
//         ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response.status(Status.BadRequest))
//       case ex =>
//         ZIO.logErrorCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response.status(Status.InternalServerError))
//     } @@ LogAspect.logAnnotateRequestData(req)
// )

// def correctReceiptHandler(req: Request) = Handler.fromFunctionZIO[Response](res =>
//   ZIO
//     .service[TollManager]
//     .zip(ZIO.service[SessionManager])
//     .zip(ZIO.attempt(req.rawHeader(SessionManager.sessionKeyHeaderName).get))
//     .flatMap((tollManager, sessionManager, sessionKey) =>
//       for {
//         tollChallenge <- tollManager.getToll(tollManager.getChallengeCost(getNextChallengeType(req)))
//         session <- sessionManager.getSession(sessionKey)
//         _ <- sessionManager.saveSession(session + (TollManager.tollChallengeContentKey, tollChallenge.toJson))
//       } yield 
//         Handler.response(
//           res
//             .addHeader(TollManager.tollHeader, tollChallenge.toll.toString)
//             .addHeader(TollManager.tollCostHeader, tollChallenge.cost.toString)
//         )
//     )
//     .catchSome {
//       case ex =>
//         ZIO
//           .logWarningCause(s"${ex.getMessage()}", Cause.fail(ex))
//           .flatMap(_ => ZIO.fail(ex))
//     }
// )