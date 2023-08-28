package is.clipperz.backend.apis

import zio.ZIO
import zio.http.{ Http, Method, Path, PathSyntax, Response, Request }
import zio.http.* //TODO: fix How do you import `Root` and `/`?
import is.clipperz.backend.services.SessionManager
import is.clipperz.backend.Main.ClipperzHttpApp
import is.clipperz.backend.exceptions.BadRequestException
import is.clipperz.backend.functions.responseTimer
import java.util
import zio.Cause
import is.clipperz.backend.LogAspect

val logoutApi: ClipperzHttpApp = Http.collectZIO[Request]:
  case request @ Method.POST -> Root / "api" / "logout" =>
    responseTimer("logout", request.method)(
      ZIO
        .service[SessionManager]
        .flatMap((sessionManager) =>
          sessionManager
            .deleteSession(request)
            .map(_ => Response.text(""))
        )
        .catchSome {
          case ex: BadRequestException =>
            ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
          case ex: NoSuchElementException =>
            ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
        }
    ) @@ LogAspect.logAnnotateRequestData(request)
