package is.clipperz.backend.apis

import java.io.FileNotFoundException
import zio.ZIO
import zio.stream.ZStream
import zio.http.{ Headers, Http, Body, /*HeaderValues,*/ Method, Path, PathSyntax, Response, Status }
import zio.http.* //TODO: fix How do you import `!!` and `/`?
import is.clipperz.backend.data.HexString
import is.clipperz.backend.exceptions.{
  NonWritableArchiveException,
  NonReadableArchiveException,
  FailedConversionException,
  ResourceNotFoundException,
}
import is.clipperz.backend.functions.fromStream
import is.clipperz.backend.services.{ BlobArchive, SaveBlobData }
import is.clipperz.backend.Main.ClipperzHttpApp
import is.clipperz.backend.exceptions.EmptyContentException
import is.clipperz.backend.exceptions.BadRequestException
import zio.Cause
import is.clipperz.backend.LogAspect
import zio.http.codec.HeaderCodec
import zio.http.Header.ContentTransferEncoding
import zio.stream.ZSink
import zio.http.Header.ContentType

val blobsApi: ClipperzHttpApp = Http.collectZIO[Request] {
  case request @ Method.POST -> !! / "blobs" =>
    ZIO
      .service[BlobArchive]
      .zip(request.body.asMultipartFormStream)
      .flatMap((archive, stream) =>
        stream.fields
          .filter(field => field.name == "blob")
          .run(ZSink.last)
          .flatMap(field => 
            field match {
              case Some(FormField.StreamingBinary(_, _, _, filename, data)) =>
                ZIO.attempt(HexString(filename.get))
                   .flatMap(hash => archive.saveBlob(hash, data))
              case _ =>
                ZIO.fail(new BadRequestException("Parameter 'file' must be a binary file"))
            }
          )
      )
      .map(results => Response.text(s"${results}"))
      .catchSome {
        case ex: EmptyContentException =>
          ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
        case ex: BadRequestException =>
          ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
        case ex: NonWritableArchiveException =>
          ZIO.logFatalCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.InternalServerError))
        case ex: FailedConversionException =>
          ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
        case ex => ZIO.logFatalCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.InternalServerError))
      } @@ LogAspect.logAnnotateRequestData(request)

  case request @ Method.DELETE -> !! / "blobs" / hash =>
    ZIO
      .service[BlobArchive]
      .zip(request.body.asMultipartFormStream)
      .flatMap((archive, stream) =>
        stream.fields
          .filter(field => field.name == "blob")
          .run(ZSink.last)
          .flatMap(field => 
            field match {
              case Some(FormField.StreamingBinary(_, _, _, filename, data)) =>
                ZIO.attempt(HexString(filename.get))
                   .flatMap(hash => archive.deleteBlob(hash, data))
              case _ =>
                ZIO.fail(new BadRequestException("Parameter 'file' must be a binary file"))
            }
          )
      )
      .map {
        case true  => Response.ok
        case false => Response(status = Status.NotFound)
      } 
      .catchSome {
        case ex: BadRequestException =>
          ZIO.logInfoCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
        case ex: NonWritableArchiveException =>
          ZIO.logFatalCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.InternalServerError))
        case ex: FailedConversionException =>
          ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
        case ex => ZIO.logFatalCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.InternalServerError))
      } @@ LogAspect.logAnnotateRequestData(request)

  case request @ Method.GET -> !! / "blobs" / hash =>
    ZIO
      .service[BlobArchive]
      .flatMap(archive => archive.getBlob(HexString(hash)))
      .map((bytes: ZStream[Any, Throwable, Byte]) =>
        Response(
          status = Status.Ok,
          body = Body.fromStream(bytes),
          headers = Headers(ContentTransferEncoding.Binary)
                      .addHeader("Content-Type", "application/octet-stream"),
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
