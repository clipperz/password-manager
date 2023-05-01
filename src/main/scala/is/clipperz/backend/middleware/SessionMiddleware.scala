package is.clipperz.backend.middleware

import zio.ZIO
import zio.json.EncoderOps
import zio.http.{ Headers, Http, Request, Response, Status }
import zio.http.* //TODO: fix How do you import `!!` and `/`?
import is.clipperz.backend.data.HexString
import is.clipperz.backend.functions.{ fromString, extractPath }
import is.clipperz.backend.services.{ ChallengeType, SessionManager, TollManager, TollChallenge }
import is.clipperz.backend.Main.ClipperzHttpApp

type SessionMiddleware = RequestHandlerMiddleware[Nothing, SessionManager, Throwable, Any]

def verifySessionNecessity(req: Request): Boolean =
  List("users", "login", "blobs", "logout").contains(extractPath(req))

def verifyNonEmptySessionNecessity(req: Request): Boolean =
  List("blobs", "logout").contains(extractPath(req))
  || (List(Method.PUT, Method.GET, Method.DELETE).contains(req.method)
  && List("users").contains(extractPath(req)))

val presentSessionHeaderMiddleware: SessionMiddleware =
  RequestHandlerMiddlewares
    .ifRequestThenElseZIO(req =>
      ZIO
        .service[SessionManager]
        .flatMap((sessionManager) => sessionManager.getSession(req))
        .map(_.isEmpty)
    )(
      RequestHandlerMiddlewares.updateResponse(res => res.withStatus(Status.Unauthorized)),
      RequestHandlerMiddleware.identity,
    )
    .when(verifyNonEmptySessionNecessity)

val missingSessionHeaderMiddleware: SessionMiddleware =
  RequestHandlerMiddlewares.updateResponse(res => res.withStatus(Status.BadRequest))

val sessionChecks: SessionMiddleware = RequestHandlerMiddlewares
  .ifRequestThenElse[SessionManager, Throwable](req => req.headers.hasHeader(SessionManager.sessionKeyHeaderName))(
    presentSessionHeaderMiddleware,
    missingSessionHeaderMiddleware,
  )
  .when(verifySessionNecessity)
