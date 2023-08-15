package is.clipperz.backend.apis

import com.github.nscala_time.time.Imports.*

import java.util
import java.time.LocalDate
import java.nio.charset.StandardCharsets

import is.clipperz.backend.data.HexString
import is.clipperz.backend.exceptions.{ BadRequestException, FailedConversionException, ResourceNotFoundException, NonWritableArchiveException, ResourceExpiredException }
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
import is.clipperz.backend.services.OneTimeSecret
import java.time.format.DateTimeFormatter
import java.time.format.FormatStyle
import java.time.format.DateTimeParseException

// ------------------------------------------------------------------------------------

case class OneTimeSecretData (
  secret: HexString,
  duration: Double,
  version: String
)

object OneTimeSecretData:
  implicit val decoder: JsonDecoder[OneTimeSecretData] = DeriveJsonDecoder.gen[OneTimeSecretData]
  implicit val encoder: JsonEncoder[OneTimeSecretData] = DeriveJsonEncoder.gen[OneTimeSecretData]

// ------------------------------------------------------------------------------------

val oneTimeShareApi: ClipperzHttpApp = Http.collectZIO[Request] {
  case request @ Method.POST -> Root / "api" / "share" =>
    ZIO
      .service[OneTimeShareArchive]
      // .zip(request.body.asMultipartForm)
      .zip(ZIO.succeed(request.body.asStream))
      .flatMap((archive, stream) =>
        fromStream[OneTimeSecretData](stream)
          .map ((secretData: OneTimeSecretData) => 
            val start = DateTime.now().withZone(DateTimeZone.UTC).nn
            OneTimeSecret(secretData.secret, start + secretData.duration.toLong, Option(secretData.version))
          )
          .flatMap ((secret: OneTimeSecret) =>
            archive.saveSecret(ZStream.fromChunks(Chunk.fromArray((secret).toJson.getBytes(StandardCharsets.UTF_8).nn)))
          )
      )
      .map(id => Response.text(s"${id}"))
      .catchAll {
        case ex: BadRequestException =>
          ZIO.logFatalCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
        case ex: NonWritableArchiveException =>
          ZIO.logFatalCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.InternalServerError))
        case ex: FailedConversionException =>
          ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
        case e => ZIO.logFatalCause(s"${e.getMessage()}", Cause.fail(e)).flatMap(_ => ZIO.fail(e))
      } @@ LogAspect.logAnnotateRequestData(request)

  case request @ Method.GET -> Root / "api" / "redeem" / id =>
    ZIO
      .service[OneTimeShareArchive]
      .flatMap(archive =>
        archive.getSecret(id).flatMap(oneTimeSecret => 
            if (oneTimeSecret.expirationDate < DateTime.now()) {
              archive.deleteSecret(id).flatMap(_ =>
                ZIO.fail(new ResourceExpiredException("Secret Expired"))
              )
            } else {
              ZIO.succeed(
                ( oneTimeSecret.version
                , ZStream
                    .fromChunk(Chunk.fromArray(oneTimeSecret.secret.toByteArray))
                    .ensuring(archive.deleteSecret(id).isSuccess)
                )
              )
            }
        )
      )
      .map((version: Option[String], bytes: ZStream[Any, Throwable, Byte]) => 
        Response(
          status = Status.Ok,
          headers = version.map(v => Headers("clipperz-oneTimeSecret-version", v)).getOrElse(Headers.empty),
          body = Body.fromStream(bytes),
        )
      )
      .catchAll {
        case ex: ResourceExpiredException => 
          ZIO.logInfo(s"${ex.getMessage()}").as(Response(status = Status.Gone))
        case ex: ResourceNotFoundException =>
          ZIO.logInfo(s"${ex.getMessage()}").as(Response(status = Status.NotFound))
        case ex: NonWritableArchiveException =>
          ZIO.logFatalCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.InternalServerError))
        case ex: FailedConversionException =>
          ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.InternalServerError))
        case ex: DateTimeParseException =>
          ZIO.logFatalCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.InternalServerError))
        case e => ZIO.logFatalCause(s"${e.getMessage()}", Cause.fail(e)).flatMap(_ => ZIO.fail(e))
      } @@ LogAspect.logAnnotateRequestData(request)

}