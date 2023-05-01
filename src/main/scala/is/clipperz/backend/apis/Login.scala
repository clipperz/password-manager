package is.clipperz.backend.apis

import zio.ZIO
import zio.json.EncoderOps
import zio.http.{ Http, Method, Path, PathSyntax, Response, Status }
import zio.http.* //TODO: fix How do you import `!!` and `/`?
import is.clipperz.backend.data.HexString
import is.clipperz.backend.functions.fromStream
import is.clipperz.backend.services.{ SessionManager, SrpManager, SRPStep1Data, SRPStep2Data }
import is.clipperz.backend.Main.ClipperzHttpApp
import is.clipperz.backend.exceptions.{ BadRequestException, FailedConversionException, ResourceNotFoundException }
import java.util
import zio.Cause
import is.clipperz.backend.LogAspect
import zio.http.Header.HeaderType

val loginApi: ClipperzHttpApp = Http.collectZIO[Request] {
  case request @ Method.POST -> !! / "login" / "step1" / c =>
    ZIO
      .service[SessionManager]
      .zip(ZIO.service[SrpManager])
      .zip(ZIO.succeed(request.body.asStream))
      .flatMap((sessionManager, srpManager, content) =>
        fromStream[SRPStep1Data](content)
          .flatMap { loginStep1Data =>
            if HexString(c) == loginStep1Data.c then
              for {
                session <- sessionManager.getSession(request) // create new session
                (step1Response, session) <- srpManager.srpStep1(loginStep1Data, session)
                _ <- sessionManager.saveSession(session)
              } yield step1Response
            else ZIO.fail(new BadRequestException("c in request path differs from c in request body "))
          }
      )
      .map(step1Response => Response.json(step1Response.toJson))
      .catchSome {
        case ex: ResourceNotFoundException =>
          ZIO.logInfo(s"${ex.getMessage()}").as(Response(status = Status.NotFound))
        case ex: BadRequestException =>
          ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
        case ex: FailedConversionException =>
          ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
        case ex: NoSuchElementException =>
          ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
      } @@ LogAspect.logAnnotateRequestData(request)

  case request @ Method.POST -> !! / "login" / "step2" / c =>
    ZIO
      .service[SessionManager]
      .zip(ZIO.service[SrpManager])
      .zip(ZIO.succeed(request.body.asStream))
      .flatMap((sessionManager, srpManager, content) =>
        fromStream[SRPStep2Data](content)
          .flatMap { loginStep2Data =>
            for {
              session <- sessionManager.getSession(request)
              (step2Response, session) <- srpManager.srpStep2(loginStep2Data, session)
              _ <- sessionManager.saveSession(session)
            } yield step2Response
          }
      )
      .map(step2Response => Response.json(step2Response.toJson))
      .catchSome {
        case ex: ResourceNotFoundException =>
          ZIO.logInfo(s"${ex.getMessage()}").as(Response(status = Status.NotFound))
        case ex: FailedConversionException =>
          ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
        case ex: BadRequestException =>
          ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
        case ex: NoSuchElementException =>
          ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
      } @@ LogAspect.logAnnotateRequestData(request)
}
