package is.clipperz.backend.apis

import is.clipperz.backend.Exceptions.*
import is.clipperz.backend.LogAspect
import is.clipperz.backend.Main.ClipperzHttpApp
import is.clipperz.backend.services.SessionManager

import java.util

import zio.{ ZIO, Cause }
import zio.http.{ Method, Path, Response, Request, Routes, handler }

val logoutApi = Routes(
    Method.POST / "api" / "logout" -> handler: (request: Request) =>
        (for {
            sessionManager <- ZIO.service[SessionManager]
            _              <- sessionManager.deleteSession(request)
        } yield Response.ok) @@ LogAspect.logAnnotateRequestData(request)
)