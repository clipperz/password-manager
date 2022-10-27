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

type SessionMiddleware = HttpMiddleware[SessionManager, Throwable]

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
        .zip(ZIO.attempt(req.headers.headerValue(SessionManager.sessionKeyHeaderName).get))
        .flatMap((sessionManager, sessionKey) => sessionManager.getSession(sessionKey))
        .map(_.isEmpty)
    )(
      Middleware.fromHttp(Http.response(Response(status = Status.Unauthorized))),
      Middleware.patch(_ => Patch.empty),
    )
    .when(verifyNonEmptySessionNecessity)

val missingSessionHeaderMiddleware: Request => SessionMiddleware = req =>
  Middleware.fromHttp(
    Http.response(Response(status = Status.BadRequest))
  )

val sessionChecks: SessionMiddleware = Middleware
  .ifThenElse[Request](req => req.headers.hasHeader(SessionManager.sessionKeyHeaderName))(
    _ => presentSessionHeaderMiddleware,
    missingSessionHeaderMiddleware,
  )
  .when(verifySessionNecessity)
