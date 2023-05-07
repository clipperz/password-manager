package is.clipperz.backend.apis

import zio.ZIO
import zio.json.EncoderOps
import zio.http.{ Http, Method, Path, PathSyntax, Response, Request, Status }
import zio.http.* //TODO: fix How do you import `!!` and `/`?
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
import is.clipperz.backend.exceptions.NonWritableArchiveException
import zio.json.JsonDecoder
import zio.json.DeriveJsonDecoder
import zio.json.JsonEncoder
import zio.json.DeriveJsonEncoder
import zio.stream.ZSink
import zio.Chunk
import zio.http.codec.HeaderCodec

// ------------------------------------------------------------------------------------

val oneTimeShareApi: ClipperzHttpApp = Http.collectZIO[Request] {
  case request @ Method.POST -> !! / "share" =>
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

  case request @ Method.GET -> !! / "redeem" / id =>
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