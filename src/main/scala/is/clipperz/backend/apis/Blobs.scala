package is.clipperz.backend.apis

import java.io.FileNotFoundException
import is.clipperz.backend.data.HexString
import is.clipperz.backend.exceptions.{
  NonWritableArchiveException,
  NonReadableArchiveException,
  FailedConversionException,
  ResourceNotFoundException,
}
import is.clipperz.backend.exceptions.EmptyContentException
import is.clipperz.backend.exceptions.BadRequestException
import is.clipperz.backend.functions.fromStream
import is.clipperz.backend.services.{ BlobArchive, SaveBlobData }
import is.clipperz.backend.Main.ClipperzHttpApp
import is.clipperz.backend.LogAspect

import zio.{ ZIO, Cause }
import zio.http.{ Headers, Http, Body, Method, Path, PathSyntax, Response, Request, Status }
import zio.http.*
import zio.http.Header.{ ContentType, ContentTransferEncoding }
import zio.http.codec.HeaderCodec
import zio.stream.{ ZStream, ZSink }

val blobsApi: ClipperzHttpApp = Http.collectZIO[Request]:
  case request @ Method.POST -> Root / "api" / "blobs" =>
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

  case request @ Method.DELETE -> Root / "api" / "blobs" / hash =>
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
      .map:
        case true  => Response.ok
        case false => Response(status = Status.NotFound)
      .catchSome {
        case ex: BadRequestException =>
          ZIO.logInfoCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
        case ex: NonWritableArchiveException =>
          ZIO.logFatalCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.InternalServerError))
        case ex: FailedConversionException =>
          ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
        case ex => ZIO.logFatalCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.InternalServerError))
      } @@ LogAspect.logAnnotateRequestData(request)

  case request @ Method.GET -> Root / "api" / "blobs" / hash =>
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
