package is.clipperz.backend.apis

import com.github.nscala_time.time.Imports.*

import java.util
import java.time.LocalDate
import java.time.format.DateTimeParseException
// import java.nio.charset.StandardCharsets

import is.clipperz.backend.Main.ClipperzHttpApp
import is.clipperz.backend.LogAspect
import is.clipperz.backend.data.HexString
import is.clipperz.backend.Exceptions.*
import is.clipperz.backend.functions.fromStream
import is.clipperz.backend.services.{ SessionManager, SrpManager, SRPStep1Data, SRPStep2Data, OneTimeShareArchive, OneTimeSecret }

import zio.{ ZIO, Cause, Chunk }
import zio.metrics.Metric
import zio.http.{ Method, Path, Response, Request, Status, Routes, Headers, Body, handler, string }
import zio.json.{ EncoderOps, JsonDecoder, DeriveJsonDecoder, JsonEncoder, DeriveJsonEncoder }
import zio.stream.{ ZStream }
import zio.nio.charset.Charset

// ------------------------------------------------------------------------------------

case class OneTimeSecretData (
    secret: HexString,
    duration: Double,
    version: SecretVersion
)

case class SecretVersion(
    tag: String
)

object SecretVersion:
    implicit val decoder: JsonDecoder[SecretVersion] = DeriveJsonDecoder.gen[SecretVersion]
    implicit val encoder: JsonEncoder[SecretVersion] = DeriveJsonEncoder.gen[SecretVersion]

object OneTimeSecretData:
    implicit val decoder: JsonDecoder[OneTimeSecretData] = DeriveJsonDecoder.gen[OneTimeSecretData]
    implicit val encoder: JsonEncoder[OneTimeSecretData] = DeriveJsonEncoder.gen[OneTimeSecretData]

// ------------------------------------------------------------------------------------

val oneTimeShareApi = Routes (
    Method.POST / "api" / "share" -> handler : (request: Request) =>
        ZIO
        .service[OneTimeShareArchive]
        .zip(ZIO.succeed(request.body.asStream))
        .flatMap((archive: OneTimeShareArchive, stream: ZStream[Any, Throwable, Byte]) =>
            fromStream[OneTimeSecretData](stream)
            .map ((secretData: OneTimeSecretData) => 
                val start = DateTime.now().withZone(DateTimeZone.UTC).nn
                OneTimeSecret(secretData.secret, start + secretData.duration.toLong, Option(secretData.version))
            )
            .flatMap ((secret: OneTimeSecret) =>
                Charset.Standard.utf8.encodeString(secret.toJson)
                .flatMap(data => archive.saveSecret(ZStream.fromChunks(data)))
                // archive.saveSecret(ZStream.fromChunks(Chunk.fromArray((secret).toJson.getBytes(StandardCharsets.UTF_8).nn)))
            )
        ) 
        .map(id => Response.text(s"${id}")) @@ LogAspect.logAnnotateRequestData(request)
    ,
    Method.GET / "api" / "redeem" / string("id") -> handler : (id: String, request: Request)=>
        ZIO
        .service[OneTimeShareArchive]
        .flatMap(archive =>
            archive.getSecret(id).flatMap(oneTimeSecret => 
                if (oneTimeSecret.expirationDate < DateTime.now())
                then
                    archive.deleteSecret(id).flatMap(_ =>
                        ZIO.fail(new ResourceExpiredException("Secret Expired"))
                    )
                else
                    ZIO.succeed(
                        ( oneTimeSecret.version
                        , ZStream
                            .fromChunk(Chunk.fromArray(oneTimeSecret.secret.toByteArray))
                            .ensuring(archive.deleteSecret(id).isSuccess)
                        )
                    )
            )
        )
        .map((version: Option[SecretVersion], bytes: ZStream[Any, Throwable, Byte]) => 
            Response(
                status  = Status.Ok,
                headers = version.map(v => Headers("clipperz-onetimesecret-version", v.toJson)).getOrElse(Headers.empty),
                body    = Body.fromStream(bytes),
            )
        ) @@ LogAspect.logAnnotateRequestData(request)
)