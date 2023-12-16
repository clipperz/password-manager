package is.clipperz.backend.middleware

import zio.ZIO
import zio.json.EncoderOps
import zio.http.{ Headers, Request, Response, Status }
import zio.http.* //TODO: fix How do you import `!!` and `/`?
import is.clipperz.backend.data.HexString
import is.clipperz.backend.functions.{ fromString, extractPath }
import is.clipperz.backend.services.{ ChallengeType, SessionManager, TollManager, TollChallenge }
import is.clipperz.backend.Main.ClipperzHttpApp

type SessionMiddleware = HandlerAspect[SessionManager, Any]

def verifySessionNecessity(req: Request): Boolean =
  List("users", "login", "blobs", "logout").contains(extractPath(req))

def verifyNonEmptySessionNecessity(req: Request): Boolean =
  List("blobs", "logout").contains(extractPath(req))
  || (List(Method.PUT, Method.GET, Method.DELETE).contains(req.method)
  && List("users").contains(extractPath(req)))

val presentSessionHeaderMiddleware: SessionMiddleware =
  Middleware
    .ifRequestThenElseZIO(req =>
      ZIO
        .service[SessionManager]
        .flatMap((sessionManager) => sessionManager.getSession(req))
        .map(_.isEmpty)
        .mapError(err => Response(status = Status.InternalServerError))
    )(
      Middleware.updateResponse(res => res.status(Status.Unauthorized)),
      Middleware.identity,
    )
    .when(verifyNonEmptySessionNecessity)

val missingSessionHeaderMiddleware: SessionMiddleware =
  Middleware.updateResponse(res => res.status(Status.BadRequest))

val sessionChecks: SessionMiddleware = 
    Middleware
    .ifRequestThenElse(req => req.headers.hasHeader(SessionManager.sessionKeyHeaderName))(
        presentSessionHeaderMiddleware,
        missingSessionHeaderMiddleware,
    )
    .when(verifySessionNecessity)
