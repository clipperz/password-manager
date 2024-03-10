package is.clipperz.backend.apis

import is.clipperz.backend.data.HexString
import is.clipperz.backend.data.HexString.bytesToHex
import is.clipperz.backend.Exceptions.*
import is.clipperz.backend.functions.{ fromStream }
import is.clipperz.backend.LogAspect
import is.clipperz.backend.services.{ BlobArchive }

import zio.{ ZIO, Cause, Chunk }
import zio.http.{ Headers, Body, Method, FormField, Path, Response, Request, Routes, Status, handler }
import zio.http.codec.HeaderCodec
import zio.http.codec.PathCodec.string
import zio.http.Header.{ ContentType, ContentTransferEncoding }
import zio.stream.{ ZStream, ZSink }
import zio.nio.file.{ Files, Path as PathNIO }

private case class BlobData(identifier: Option[Identifier], blob: Option[Blob])
private case class Identifier(value: HexString)
private case class Blob(hash: HexString, data: ZStream[Any, Nothing, Byte])
// private case class Blob(hash: HexString, data: PathNIO)

val blobsApi: Routes[BlobArchive, Throwable] = Routes(
    Method.POST / "api" / "blobs" -> handler: (request: Request) =>
        ZIO.service[BlobArchive]
        .zip(request.body.asMultipartFormStream)
        .flatMap((archive, streamingForm) => 
            streamingForm.fields
            .collectZIO(field => field match {
                case FormField.StreamingBinary("identifier",        contentType, transferEncoding,      filename,  data) => data.run(ZSink.collectAll[Byte]).map(_.toArray).map(bytes => Identifier(bytesToHex(bytes)))
                case FormField.StreamingBinary("blob",              contentType, transferEncoding, Some(filename), data) => ZIO.succeed(Blob(HexString(filename), data))
            })
            .runFoldZIO(BlobData(None, None))((result, field) => field match {
                case Identifier(value) =>   if result.identifier == None
                                            then ZIO.succeed(BlobData(Some(Identifier(value)), result.blob))
                                            else ZIO.fail(new BadRequestException(s"Parameter 'identifier' specified more than once"))
                case Blob(hash, data)  =>   if result.blob == None
                                            then ZIO.succeed(BlobData(result.identifier, Some(Blob(hash, data))))
                                            else ZIO.fail(new BadRequestException(s"Parameter 'blob' specified more than once"))
            })
            .flatMap(blobData => blobData match {
                case BlobData(Some(Identifier(identifier)), Some(Blob(hash, data))) => archive.saveBlob(hash, identifier, data)
                case _                                                              => ZIO.fail(new BadRequestException(s"Missing either/both 'blob', 'identifier' fields"))
            })
        )
        .map(result => Response.ok)
        // @@ LogAspect.logAnnotateRequestData(request)
,
    Method.DELETE / "api" / "blobs" / string("hash") -> handler: (hash: String, request: Request) =>
        ZIO
        .service[BlobArchive]
        .zip(request.body.asMultipartFormStream)
        .flatMap((archive, streamingForm) => streamingForm
            .fields
            // .tap(field => ZIO.log(s"DELETE FIELD: ${field}"))
            .collectZIO(field => field match {
                case FormField.StreamingBinary("identifier", contentType, transferEncoding,      filename,  data)   =>  data.run(ZSink.collectAll[Byte])
                                                                                                                            .map(_.toArray)
                                                                                                                            .flatMap(identifierBytes => archive.deleteBlob(HexString(hash), bytesToHex(identifierBytes)))
            })
            .runCount
        )
        .map:
            case 1  => Response.ok
            case _  => Response(status = Status.NotFound)
        // @@ LogAspect.logAnnotateRequestData(request)
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
        // @@ LogAspect.logAnnotateRequestData(request)
)