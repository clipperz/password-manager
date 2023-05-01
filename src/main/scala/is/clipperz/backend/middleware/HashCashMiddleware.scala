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

def verifyRequestToll(request: Request): ZIO[TollManager & SessionManager, Throwable, (Boolean, TollChallenge)] =
  ZIO.service[SessionManager].zip(ZIO.service[TollManager])
    .flatMap { (sessionManager, tollManager) =>
      sessionManager.getSession(request).flatMap(session => {
        var tollIsValid = for {
          challengeJson     <-  ZIO.attempt(session(TollManager.tollChallengeContentKey).get)
                                  .mapError(e => new BadRequestException("No challenge related to this session"))
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
          .catchSome {
            case ex: NoSuchElementException =>
              ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Handler.status(Status.BadRequest))
            case ex: BadRequestException =>
              ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Handler.status(Status.BadRequest))
            case ex =>
              ZIO.logErrorCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Handler.status(Status.InternalServerError))
          } @@ LogAspect.logAnnotateRequestData(request)
    }.flatten
}.when(isTollRequired)
