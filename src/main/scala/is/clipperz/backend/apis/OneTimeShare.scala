package is.clipperz.backend.apis

import zio.ZIO
import zio.json.EncoderOps
import zhttp.http.{ Http, Method, Path, PathSyntax, Response, Status }
import zhttp.http.* //TODO: fix How do you import `!!` and `/`?
import is.clipperz.backend.data.HexString
import is.clipperz.backend.functions.fromStream
import is.clipperz.backend.services.{ SessionManager, SrpManager, SRPStep1Data, SRPStep2Data }
import is.clipperz.backend.Main.ClipperzHttpApp
import is.clipperz.backend.exceptions.{ BadRequestException, FailedConversionException, ResourceNotFoundException }
import java.util
import zio.Cause
import is.clipperz.backend.LogAspect
import is.clipperz.backend.services.OneTimeShareArchive
import zio.stream.ZStream
import is.clipperz.backend.exceptions.ResourceConflictException
import is.clipperz.backend.exceptions.ConflictualRequestException
import is.clipperz.backend.exceptions.NonWritableArchiveException

val oneTimeShareApi: ClipperzHttpApp = Http.collectZIO {
  case request @ Method.POST -> !! / "share" =>
    ZIO
      .service[OneTimeShareArchive]
      .zip(ZIO.succeed(request.body.asStream))
      .flatMap((archive, bytes) =>
        archive.saveSecret(bytes)
      )
      .map(id => Response.text(s"${id}"))
      .catchSome {
        case ex: ResourceConflictException =>
          ZIO.logDebugCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.Conflict))
        case ex: ConflictualRequestException =>
          ZIO.logDebugCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.Conflict))
        case ex: BadRequestException =>
          ZIO.logFatalCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
        case ex: NonWritableArchiveException =>
          ZIO.logFatalCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.InternalServerError))
        case ex: FailedConversionException =>
          ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
        case ex => ZIO.logFatalCause(s"${ex.getMessage()}", Cause.fail(ex)).flatMap(_ => ZIO.fail(ex))
      } @@ LogAspect.logAnnotateRequestData(request)

  case request @ Method.POST -> !! / "reedeem" / id =>
    ZIO
      .service[SessionManager]
      .zip(ZIO.service[SrpManager])
      .zip(ZIO.attempt(request.headers.headerValue(SessionManager.sessionKeyHeaderName).get)) // TODO: return significant status in response
      .zip(ZIO.succeed(request.body.asStream))
      .flatMap((sessionManager, srpManager, sessionKey, content) =>
        fromStream[SRPStep2Data](content)
          .flatMap { loginStep2Data =>
            for {
              session <- sessionManager.getSession(sessionKey)
              // _ <- ZIO.succeed(println(s"OPTIONAL SESSION: ${optionalSession}"))
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
          ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.Forbidden))
        case ex: NoSuchElementException =>
          ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
      } @@ LogAspect.logAnnotateRequestData(request)
}
