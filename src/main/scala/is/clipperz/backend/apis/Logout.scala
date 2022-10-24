package is.clipperz.backend.apis

import zio.{ ZIO }
import zhttp.http.{ Http, Method, Path, PathSyntax, Response }
import zhttp.http.* //TODO: fix How do you import `!!` and `/`?
import is.clipperz.backend.services.{ SessionManager }
import is.clipperz.backend.Main.ClipperzHttpApp
import is.clipperz.backend.exceptions.BadRequestException

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
        case ex : BadRequestException => ZIO.succeed(Response(status = Status.BadRequest))
      }
}
