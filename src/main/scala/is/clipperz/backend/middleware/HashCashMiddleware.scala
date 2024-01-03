package is.clipperz.backend.middleware

import java.util.NoSuchElementException
import is.clipperz.backend.data.HexString
import is.clipperz.backend.functions.{ fromString, extractPath, customMapError }
import is.clipperz.backend.services.{ ChallengeType, SessionManager, TollManager, TollChallenge }
import is.clipperz.backend.Main.ClipperzHttpApp
import is.clipperz.backend.exceptions.BadRequestException
import is.clipperz.backend.LogAspect
import is.clipperz.backend.services.PRNG
import is.clipperz.backend.services.UserArchive
import is.clipperz.backend.services.BlobArchive
import is.clipperz.backend.services.OneTimeShareArchive
import is.clipperz.backend.services.SrpManager
import zio.{ZIO, Cause, Trace}
import zio.json.EncoderOps
import zio.http.{ Headers, Request, Response, Status }
import zio.http.* //TODO: fix How do you import `!!` and `/`?
import zio.http.Handler.RequestHandlerSyntax

type TollMiddleware = HandlerAspect[TollManager & SessionManager, Any]

def verifyRequestToll(request: Request, challengeType: ChallengeType, nextChallengeType: ChallengeType) =
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
            challengeTypeForNextStep <-  ZIO.succeed(if (tollIsValid) then nextChallengeType else challengeType)
            challengeForNextStep     <-  tollManager.getToll(tollManager.getChallengeCost(challengeTypeForNextStep))
            _                        <-  sessionManager.saveSession(session + (TollManager.tollChallengeContentKey, challengeForNextStep.toJson))
          } yield (tollIsValid, challengeForNextStep)
        )
      })
    }.mapError(customMapError)

def hashcash(challengeType: ChallengeType, nextChallengeType: ChallengeType) = new Middleware[SessionManager & TollManager]:
  override def apply[Env1 <: SessionManager & TollManager, Err](routes: Routes[Env1, Err]): Routes[Env1, Err] =
    routes.transform(handler => 
        Handler.fromFunctionZIO[Request] { request =>
            verifyRequestToll(request, challengeType, nextChallengeType).tap(res => ZIO.log(s"VERIFY REQUEST TOLL RESULT: $res"))
            .map((isRequestTollValid, newToll) =>
                ( if isRequestTollValid
                  then handler
                  else Handler.status(Status.PaymentRequired)
                ).addHeader(TollManager.tollHeader,     newToll.toll.toString)
                 .addHeader(TollManager.tollCostHeader, newToll.cost.toString)
            )
        }.flatten
    )