package is.clipperz.backend.middleware

import is.clipperz.backend.data.HexString
import is.clipperz.backend.Exceptions.*
import is.clipperz.backend.functions.{ fromString, customMapError }
import is.clipperz.backend.Main.ClipperzHttpApp
import is.clipperz.backend.LogAspect
import is.clipperz.backend.services.{ ChallengeType, SessionManager, TollManager, TollChallenge, PRNG, BlobArchive, OneTimeShareArchive, SessionKey, SrpManager, UserArchive }

import java.util.NoSuchElementException

import zio.{ZIO, Cause, Trace}
import zio.http.{ Handler, HandlerAspect, Header, Headers, Middleware, Request, Response, Routes, Status }
import zio.http.Handler.RequestHandlerSyntax
import zio.json.EncoderOps

type TollMiddleware = HandlerAspect[TollManager & SessionManager, Any]

def verifyRequestToll(request: Request, challengeType: ChallengeType, nextChallengeType: ChallengeType) =
  ZIO.service[SessionManager].zip(ZIO.service[TollManager])
    .flatMap { (sessionManager, tollManager) =>
      sessionManager.getSession(request).flatMap(session => {
        (for {
          challengeJson     <-  ZIO.attempt(session(TollManager.tollChallengeContentKey).get)
          challenge         <-  fromString[TollChallenge](challengeJson)
          receipt           <-  ZIO.attempt(request.rawHeader(TollManager.tollReceiptHeader).map(HexString(_)).get)
          tollIsValid       <-  tollManager.verifyToll(challenge, receipt)
        } yield tollIsValid)
        .catchAll(_ => ZIO.succeed(false))
        .flatMap(tollIsValid => for {
            challengeTypeForNextStep <-  ZIO.succeed(if (tollIsValid) then nextChallengeType else challengeType)
            challengeForNextStep     <-  tollManager.getToll(tollManager.getChallengeCost(challengeTypeForNextStep))
            _                        <-  sessionManager.saveSession(session + (TollManager.tollChallengeContentKey, challengeForNextStep.toJson))
          } yield (tollIsValid, challengeForNextStep, session.key)
        )
      })
    }.mapError(customMapError) @@ LogAspect.logAnnotateRequestData(request)

def hashcash(challengeType: ChallengeType, nextChallengeType: ChallengeType) = new Middleware[SessionManager & TollManager]:
  override def apply[Env1 <: SessionManager & TollManager, Err](routes: Routes[Env1, Err]): Routes[Env1, Err] =
    routes.transform(handler => 
        Handler.fromFunctionZIO[Request] { request =>
            verifyRequestToll(request, challengeType, nextChallengeType)
            .map((isRequestTollValid, newToll, sessionKey) =>
                ( if isRequestTollValid
                  then handler
                  else Handler
                        .status(Status.PaymentRequired)
                        .addHeader(Header.Connection.Close)
                ).addHeader(TollManager.tollHeader,              newToll.toll.toString)
                 .addHeader(TollManager.tollCostHeader,          newToll.cost.toString)
                 .addHeader(SessionManager.sessionKeyHeaderName, sessionKey)
                 
            )
        }.flatten
    )