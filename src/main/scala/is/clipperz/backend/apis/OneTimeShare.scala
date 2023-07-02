package is.clipperz.backend.apis

import java.util
import java.time.LocalDate
import java.nio.charset.StandardCharsets

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
import is.clipperz.backend.services.OneTimeSecret
import java.time.format.DateTimeFormatter
import java.time.format.FormatStyle
import java.time.format.DateTimeParseException

// ------------------------------------------------------------------------------------

case class OneTimeSecretInfo (
  creationDate:   String,
  expirationDate: String
)

object OneTimeSecretInfo:
  implicit val decoder: JsonDecoder[OneTimeSecretInfo] = DeriveJsonDecoder.gen[OneTimeSecretInfo]
  implicit val encoder: JsonEncoder[OneTimeSecretInfo] = DeriveJsonEncoder.gen[OneTimeSecretInfo]

// ------------------------------------------------------------------------------------

val oneTimeShareApi: ClipperzHttpApp = Http.collectZIO[Request] {
  case request @ Method.POST -> Root / "api" / "share" =>
    ZIO
      .service[OneTimeShareArchive]
      .zip(ZIO.succeed(request.body.asStream))
      .flatMap((archive, stream) =>
        fromStream[OneTimeSecret](stream)
          .flatMap ((secret: OneTimeSecret) => 
            archive.saveSecret(ZStream.fromChunks(Chunk.fromArray((secret).toJson.getBytes(StandardCharsets.UTF_8).nn)))
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

  case request @ Method.GET -> Root / "api" / "oneTimeSecretInfo" / id =>
    ZIO
      .service[OneTimeShareArchive]
      .flatMap(archive => {
        archive.getSecret(id)
          .map((oneTimeSecret) => {
            val secretData = OneTimeSecretInfo(oneTimeSecret.creationDate, oneTimeSecret.expirationDate)
            // if (LocalDate
            //       .parse(oneTimeSecret.expirationDate, DateTimeFormatter.ofLocalizedDateTime(FormatStyle.SHORT, FormatStyle.SHORT)).nn
            //       .isBefore(LocalDate.now())
            // ) {
              // archive.deleteSecret(id)
            // }
            secretData
        })
      }).map(secretData => 
        Response.json(secretData.toJson)
      )
      .catchSome {
        case ex: ResourceNotFoundException =>
          ZIO.logInfo(s"${ex.getMessage()}").as(Response(status = Status.NotFound))
        case ex: NonWritableArchiveException =>
          ZIO.logFatalCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.InternalServerError))
        case ex: FailedConversionException =>
          ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.InternalServerError))
      } @@ LogAspect.logAnnotateRequestData(request)

  case request @ Method.GET -> Root / "api" / "redeem" / id =>
    ZIO
      .service[OneTimeShareArchive]
      .flatMap(archive =>
        archive.getSecret(id).flatMap(oneTimeSecret => 
          // if (LocalDate
          //         .parse(oneTimeSecret.expirationDate, DateTimeFormatter.ofLocalizedDateTime(FormatStyle.SHORT, FormatStyle.SHORT)).nn
          //         .isBefore(LocalDate.now())
          // ) {
          //   archive.deleteSecret(id)
          //   ZIO.fail(new ResourceNotFoundException("Secret Expired"))
          // } else {
            ZIO.succeed(ZStream
              .fromChunk(Chunk.fromArray(oneTimeSecret.secret.toByteArray))
              .ensuring(archive.deleteSecret(id).isSuccess))
          // }
        )
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
        case ex: DateTimeParseException =>
          ZIO.logFatalCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.InternalServerError))
      } @@ LogAspect.logAnnotateRequestData(request)

}