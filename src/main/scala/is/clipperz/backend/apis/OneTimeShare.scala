package is.clipperz.backend.apis

import java.util

import is.clipperz.backend.data.HexString
import is.clipperz.backend.exceptions.{ BadRequestException, FailedConversionException, ResourceNotFoundException, NonWritableArchiveException }
import is.clipperz.backend.functions.fromStream
import is.clipperz.backend.services.{ SessionManager, SrpManager, SRPStep1Data, SRPStep2Data }
import is.clipperz.backend.services.OneTimeShareArchive
import is.clipperz.backend.Main.ClipperzHttpApp
import is.clipperz.backend.LogAspect

import zio.{ ZIO, Cause, Chunk }
import zio.http.{ Http, Method, Path, PathSyntax, Response, Request, Status }
import zio.http.* //TODO: fix How do you import `Root` and `/`?
import zio.json.{ EncoderOps, JsonDecoder, DeriveJsonDecoder, JsonEncoder, DeriveJsonEncoder }
import zio.stream.{ ZStream, ZSink }

// ------------------------------------------------------------------------------------

val oneTimeShareApi: ClipperzHttpApp = Http.collectZIO[Request] {
  case request @ Method.POST -> Root / "api" / "share" =>
    ZIO
      .service[OneTimeShareArchive]
      .zip(request.body.asMultipartFormStream)
      .flatMap((archive, stream) =>
        stream.fields
          .filter(field => field.name == "blob")
          .run(ZSink.last)
          .flatMap(field => 
            field match {
              case Some(FormField.StreamingBinary(_, _, _, _, data)) =>
                archive.saveSecret(data)
              case _ =>
                ZIO.fail(new BadRequestException("Parameter 'file' must be a binary file"))
            }
          )
      )
      .map(id => Response.text(s"${id}"))
      .catchSome {
        case ex: BadRequestException =>
          ZIO.logFatalCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
        case ex: NonWritableArchiveException =>
          ZIO.logFatalCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.InternalServerError))
        case ex: FailedConversionException =>
          ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
        case ex => ZIO.logFatalCause(s"${ex.getMessage()}", Cause.fail(ex)).flatMap(_ => ZIO.fail(ex))
      } @@ LogAspect.logAnnotateRequestData(request)

  case request @ Method.GET -> Root / "api" / "redeem" / id =>
    ZIO
      .service[OneTimeShareArchive]
      .flatMap(archive =>
        archive.getSecret(id).map(secret => secret.ensuring(archive.deleteSecret(id).isSuccess))
      )
      .map((bytes: ZStream[Any, Throwable, Byte]) =>
        Response(
          status = Status.Ok,
          body = Body.fromStream(bytes),
        )
      )
      .catchSome {
        case ex: ResourceNotFoundException =>
          ZIO.logInfo(s"${ex.getMessage()}").as(Response(status = Status.NotFound))
        case ex: NonWritableArchiveException =>
          ZIO.logFatalCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.InternalServerError))
        case ex: FailedConversionException =>
          ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.InternalServerError))
      } @@ LogAspect.logAnnotateRequestData(request)

}