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
import zio.json.JsonDecoder
import zio.json.DeriveJsonDecoder
import zio.json.JsonEncoder
import zio.json.DeriveJsonEncoder
import zio.stream.ZSink
import zio.Chunk

// ------------------------------------------------------------------------------------

case class SaveOTSData(
    data: HexString
  )

object SaveOTSData:
  implicit val decoder: JsonDecoder[SaveOTSData] = DeriveJsonDecoder.gen[SaveOTSData]
  implicit val encoder: JsonEncoder[SaveOTSData] = DeriveJsonEncoder.gen[SaveOTSData]

// ------------------------------------------------------------------------------------

val oneTimeShareApi: ClipperzHttpApp = Http.collectZIO {
  case request @ Method.POST -> !! / "share" =>
    ZIO
      .service[OneTimeShareArchive]
      .zip(ZIO.succeed(request.body.asStream))
      .flatMap((archive, bytes) =>
        // fromStream[SaveOTSData](bytes)
        fromStream[HexString](bytes)
          .flatMap(saveData => archive.saveSecret(ZStream.fromIterable(saveData.toByteArray)))
        // archive.saveSecret(bytes)
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
          headers = Headers(HeaderNames.contentType, HeaderValues.applicationOctetStream),
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
      // .zipLeft(ZIO.service[OneTimeShareArchive].flatMap(archive => archive.deleteSecret(id))) @@ LogAspect.logAnnotateRequestData(request)

}