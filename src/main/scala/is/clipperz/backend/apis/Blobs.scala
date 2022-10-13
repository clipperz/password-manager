package is.clipperz.backend.apis

import zio.{ ZIO }
import zio.stream.{ ZStream }
import zhttp.http.{ Headers, HeaderNames, HeaderValues, Http, HttpData, Method, Path, PathSyntax, Response, Status }
import zhttp.http.* //TODO: fix How do you import `!!` and `/`?
import is.clipperz.backend.data.HexString
import is.clipperz.backend.functions.{ fromStream }
import is.clipperz.backend.services.{ BlobArchive, SaveBlobData }
import is.clipperz.backend.Main.ClipperzHttpApp

val blobsApi: ClipperzHttpApp = Http.collectZIO {
  case request @ Method.POST -> !! / "blobs" =>
    ZIO
      .service[BlobArchive]
      .zip(ZIO.succeed(request.bodyAsStream))
      .flatMap((archive, bytes) => 
        fromStream[SaveBlobData](bytes)
        .flatMap(saveData =>
          archive.saveBlob(saveData.hash, ZStream.fromIterable(saveData.data.toByteArray))
        )
      )
      .fold(
        err => { println(s"ERROR ${err}"); Response(status = Status.InternalServerError) },
        results => Response.text(s"${results}")
      )
      // .map(hash => Response.text(s"${hash}"))

  case request @ Method.DELETE -> !! / "blobs" / hash =>
    ZIO
      .service[BlobArchive]
      .zip(ZIO.succeed(request.bodyAsStream))
      .flatMap((archive, bytes) =>
        fromStream[SaveBlobData](bytes)
        .flatMap(saveData =>
          archive.deleteBlob(ZStream.fromIterable(saveData.data.toByteArray))
        )
      )
      .map(_ => Response.ok)

  case request @ Method.GET -> !! / "blobs" / hash =>
    ZIO
      .service[BlobArchive]
      .flatMap(archive => archive.getBlob(HexString(hash)))
      .map((bytes: ZStream[Any, Throwable, Byte]) =>
        Response(
          status = Status.Ok,
          data = HttpData.fromStream(bytes),
          headers = Headers(HeaderNames.contentType, HeaderValues.applicationOctetStream),
        )
      )
}
