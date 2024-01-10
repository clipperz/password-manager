package is.clipperz.backend.apis

import zio.ZIO
import zio.http.{ Method, Path, Response, Request }
import zio.http.* //TODO: fix How do you import `Root` and `/`?
import is.clipperz.backend.services.SessionManager
import is.clipperz.backend.Main.ClipperzHttpApp
import is.clipperz.backend.exceptions.BadRequestException
import java.util
import zio.Cause
import is.clipperz.backend.LogAspect

val logoutApi = Routes(
    Method.POST / "api" / "logout" -> handler: (request: Request) =>
        (for {
            sessionManager <- ZIO.service[SessionManager]
            _              <- sessionManager.deleteSession(request)
        } yield Response.ok) @@ LogAspect.logAnnotateRequestData(request)
)