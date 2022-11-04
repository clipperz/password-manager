package is.clipperz.backend.apis

import zio.ZIO
import zhttp.http.{ Http, Method, Path, PathSyntax, Response }
import zhttp.http.* //TODO: fix How do you import `!!` and `/`?
import is.clipperz.backend.services.SessionManager
import is.clipperz.backend.Main.ClipperzHttpApp
import is.clipperz.backend.exceptions.BadRequestException
import java.util
import zio.Cause
import is.clipperz.backend.LogAspect

val logoutApi: ClipperzHttpApp = Http.collectZIO {
  case request @ Method.POST -> !! / "logout" =>
    ZIO
      .service[SessionManager]
      .zip(ZIO.attempt(request.headers.headerValue(SessionManager.sessionKeyHeaderName).get))
      .flatMap((sessionManager, sessionKey) =>
        sessionManager
          .deleteSession(sessionKey)
          .map(_ => Response.text(""))
      )
      .catchSome {
        case ex: BadRequestException =>
          ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
        case ex: NoSuchElementException =>
          ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
      } @@ LogAspect.logAnnotateRequestData(request)
}
