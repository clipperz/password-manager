package is.clipperz.backend.middleware

import zio.ZIO
import zio.Task
import zio.json.EncoderOps
import zio.http.{ Headers, Request, Response, Status }
import zio.http.* //TODO: fix How do you import `!!` and `/`?
import is.clipperz.backend.data.HexString
import is.clipperz.backend.functions.{ fromString }
import is.clipperz.backend.services.{ ChallengeType, SessionManager, TollManager, TollChallenge }
import is.clipperz.backend.Main.ClipperzHttpApp
import is.clipperz.backend.services.Session
import zio.http.Status.InternalServerError
import zio.http.Status.Unauthorized
import is.clipperz.backend.functions.customErrorHandler
import is.clipperz.backend.functions.customMapError

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