package is.clipperz.backend.middleware

import is.clipperz.backend.data.HexString
import is.clipperz.backend.functions.{ customErrorHandler, customMapError, fromString }
import is.clipperz.backend.Main.ClipperzHttpApp
import is.clipperz.backend.services.{ ChallengeType, Session, SessionManager, TollManager, TollChallenge }

import zio.{ ZIO, Task }
import zio.json.EncoderOps
import zio.http.{ HandlerAspect, Headers, Middleware, Request, Response, Status }
import zio.http.Status.{ InternalServerError, Unauthorized }

type SessionMiddleware = HandlerAspect[SessionManager, Any]

def authorizedMiddleware(cExtractor: (Request) => Task[String]): SessionMiddleware =
    Middleware.ifRequestThenElseZIO(req =>
        (for {
            sessionManager <- ZIO.service[SessionManager]
            session        <- sessionManager.getSession(req)
            c              <- cExtractor(req)
        } yield sessionManager.verifySessionUser(c, session)
        ).mapError(customMapError)
    )(
        Middleware.identity
    ,   Middleware.fail(Response.status(Unauthorized))
    )