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

def isTollRequired(req: Request): Boolean =
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
/*
def isTollInSession(req: Request): ZIO[SessionManager, Throwable, Boolean] =
  ZIO
    .service[SessionManager]
    .zip(ZIO.attempt(req.rawHeader(SessionManager.sessionKeyHeaderName).get))       //  "clipperz-usersession-id"
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

val _hashcash: TollMiddleware = RequestHandlerMiddlewares
  .ifRequestThenElseFunction[TollManager & SessionManager, Throwable](req => req.hasHeader(TollManager.tollReceiptHeader))(tollPresentMiddleware, missingTollMiddleware)
  .when(verifyTollNecessity)
*/

// ============================================================================
//
//  Now we have two types of middlewares, `RequestHandlerMiddleware` transforms a `Handler` which is basically a Request => Response function,
//  while `HttpRouteMiddleware` transforms a Http which includes transforming the routing.
//  Most of the middleware use cases are request handler middlewares - one example for a route middleware is dynamically turning on/off complete
//  "routing subtrees".
//  So your use case should be a request handler middleware: it gets a Handler and returns a Handler - in the new handler you can look into the
//  request and decide to either pass the request to the original handler, or just return with a response. 
//

//  TollMiddleware = RequestHandlerMiddleware[Nothing, TollManager & SessionManager, Throwable, Any]

def verifyRequestToll(request: Request): ZIO[TollManager & SessionManager, Throwable, (Boolean, TollChallenge)] =
  ZIO.service[SessionManager].zip(ZIO.service[TollManager])
    .flatMap { (sessionManager, tollManager) =>
      sessionManager.getSession(request).flatMap(session => {
        var tollIsValid = for {
          challengeJson     <-  ZIO.attempt(session(TollManager.tollChallengeContentKey).get)
                                  .mapError(e => new BadRequestException("No challenge related to this session"))
          _                 <-  ZIO.log(s"Challenge JSON: ${challengeJson}")
          challenge         <-  fromString[TollChallenge](challengeJson)
          receipt           <-  ZIO.attempt(request.rawHeader(TollManager.tollReceiptHeader).map(HexString(_)).get)
          tollIsValid       <-  tollManager.verifyToll(challenge, receipt)
        } yield tollIsValid

        tollIsValid.catchAll(_ => ZIO.succeed(false))
        .flatMap(tollIsValid => for {
            nextChallengeType <-  ZIO.succeed(if (tollIsValid) then getNextChallengeType(request) else getChallengeType(request))
            nextChallenge     <-  tollManager.getToll(tollManager.getChallengeCost(nextChallengeType))
            _                 <-  sessionManager.saveSession(session + (TollManager.tollChallengeContentKey, nextChallenge.toJson))
          } yield (tollIsValid, nextChallenge)
        )
      })
    }

val hashcash: TollMiddleware = new RequestHandlerMiddleware.Simple[TollManager & SessionManager, Throwable] {
  override def apply[R1 <: TollManager & SessionManager, Err1 >: Throwable](handler: Handler[R1, Err1, Request, Response])(implicit trace: Trace): Handler[R1, Err1, Request, Response] =
    Handler.fromFunctionZIO[Request] { request =>
        verifyRequestToll(request)
          .map((isRequestTollValid, newToll) =>
            ( isRequestTollValid match 
                case true  => handler
                case false => Handler.status(Status.PaymentRequired)
            ) .addHeader(TollManager.tollHeader,      newToll.toll.toString)
              .addHeader(TollManager.tollCostHeader,  newToll.cost.toString)
          )
    }.flatten
}.when(isTollRequired)
  
// final def customAuthZIO[R, E](
//   verify: Headers => ZIO[R, E, Boolean],
//   responseHeaders: Headers = Headers.empty,
//   responseStatus: Status = Status.Unauthorized,
// ): RequestHandlerMiddleware[Nothing, R, E, Any] =
//   new RequestHandlerMiddleware.Simple[R, E] {
//     override def apply[R1 <: R, Err1 >: E](handler: Handler[R1, Err1, Request, Response])(implicit trace: Trace): Handler[R1, Err1, Request, Response] =
//       Handler.fromFunctionZIO[Request] { request =>
//         verify(request.headers).map {
//           case true  => handler
//           case false => Handler.status(responseStatus).addHeaders(responseHeaders)
//         }
//       }.flatten
//   }

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