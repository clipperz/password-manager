package is.clipperz.backend.apis

import java.io.FileNotFoundException

import is.clipperz.backend.data.HexString
import is.clipperz.backend.exceptions.{
    NonWritableArchiveException,
    NonReadableArchiveException,
    FailedConversionException,
    ResourceNotFoundException,
    EmptyContentException,
    BadRequestException
}
import is.clipperz.backend.functions.{ fromStream }
import is.clipperz.backend.services.{ BlobArchive }
import is.clipperz.backend.Main.ClipperzHttpApp
import is.clipperz.backend.LogAspect

import zio.{ ZIO, Cause, Chunk }
import zio.http.{ Headers, Body, Method, FormField, Path, Response, Request, Routes, Status, handler }
import zio.http.Header.{ ContentType, ContentTransferEncoding }
import zio.http.codec.HeaderCodec
import zio.http.codec.PathCodec.string
import zio.stream.{ ZStream, ZSink }

val blobsApi: Routes[BlobArchive, Throwable] = Routes(
    Method.POST / "api" / "blobs" -> handler: (request: Request) =>
        ZIO
        .service[BlobArchive]
        .zip(request.body.asMultipartFormStream)
        .flatMap((archive, stream) =>
            stream
                .fields
                .filter(_.name == "blob")
                .map(field =>
                    field match {
                        case FormField.StreamingBinary(_, _, _, filename, data) =>
                            ZIO.attempt(HexString(filename.get))
                            .flatMap(hash => archive.saveBlob(hash, data))
                        case field =>
                            ZIO.fail(new BadRequestException(s"Parameter 'blob' must be a binary file, not a ${field.getClass()}"))
                    }
                ).runFoldZIO((0, HexString("unit")))((collect, res) => res.map((collect._1 + 1, _)))
        )
        .flatMap((n, result) => 
            if n == 0 
            then ZIO.fail(new BadRequestException("No fields in form"))
            else ZIO.succeed(Response.text(s"${result}"))
        )
        @@ LogAspect.logAnnotateRequestData(request)
,
    Method.DELETE / "api" / "blobs" -> handler: (request: Request) =>
        ZIO
        .service[BlobArchive]
        .zip(request.body.asMultipartFormStream)
        .flatMap((archive, stream) =>
            stream
                .fields
                .filter(_.name == "blob")
                .map(field =>
                    field match {
                        case FormField.StreamingBinary(_, _, _, filename, data) =>
                            ZIO.attempt(HexString(filename.get))
                            .flatMap(hash => archive.deleteBlob(hash, data))
                        case field =>
                            ZIO.fail(new BadRequestException(s"Parameter 'blob' must be a binary file, not a ${field.getClass()}"))
                    }
                ).runFoldZIO(true)((collector, res) => res.map(_ && collector))
        )
        .map:
            case true  => Response.ok
            case false => Response(status = Status.NotFound)
        @@ LogAspect.logAnnotateRequestData(request)
,
    Method.GET / "api" / "blobs" / string("hash") -> handler: (hash: String, request: Request) =>
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
        @@ LogAspect.logAnnotateRequestData(request)
)