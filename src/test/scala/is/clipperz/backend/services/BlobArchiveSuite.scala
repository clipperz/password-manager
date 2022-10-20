package is.clipperz.backend.services

import org.scalacheck.Test

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Paths, FileSystems }
import java.security.MessageDigest
import scala.language.postfixOps
import zio.{ Chunk, ZIO }
import zio.stream.{ ZStream, ZSink }
import zio.test.Assertion.{ nothing }
import zio.test.{ ZIOSpecDefault, assertTrue, assert, TestAspect }
import zio.json.EncoderOps
import zhttp.http.{ Version, Headers, Method, URL, Request, HttpData }
import zhttp.http.*
import is.clipperz.backend.Main
import is.clipperz.backend.data.HexString
import is.clipperz.backend.data.HexString.{ bytesToHex }
import is.clipperz.backend.functions.crypto.HashFunction
import java.nio.file.Path
import is.clipperz.backend.functions.FileSystem

object BlobSpec extends ZIOSpecDefault:
  val app = Main.clipperzBackend
  val blobBasePath = FileSystems.getDefault().nn.getPath("target", "tests", "archive", "blobs").nn
  val userBasePath = FileSystems.getDefault().nn.getPath("target", "tests", "archive", "users").nn

  val environment =
    PRNG.live ++
    SessionManager.live ++
    UserArchive.fs(userBasePath, 2) ++
    BlobArchive.fs(blobBasePath, 2) ++
    ((UserArchive.fs(userBasePath, 2) ++ PRNG.live) >>> SrpManager.v6a()) ++
    (PRNG.live >>> TollManager.live)

  val testFile = new File(
    "src/test/resources/blobs/4073041693a9a66983e6ffb75b521310d30e6db60afc0f97d440cb816bce7c63.blob"
  )
  val blobData : SaveBlobData = SaveBlobData(
    data = bytesToHex(Files.readAllBytes(Paths.get( "src/test/resources/blobs/4073041693a9a66983e6ffb75b521310d30e6db60afc0f97d440cb816bce7c63.blob")).nn),
    hash = HexString("4073041693a9a66983e6ffb75b521310d30e6db60afc0f97d440cb816bce7c63")
  )

  val post = Request(
    url = URL(!! / "blobs"),
    method = Method.POST,
    headers = Headers.empty,
    data = HttpData.fromString(blobData.toJson, StandardCharsets.UTF_8.nn),
    version = Version.Http_1_1,
  )

  val delete = Request(
    url = URL(!! / "blobs" / "4073041693a9a66983e6ffb75b521310d30e6db60afc0f97d440cb816bce7c63"),
    method = Method.DELETE,
    headers = Headers.empty,
    data = HttpData.fromString(blobData.toJson, StandardCharsets.UTF_8.nn),
    version = Version.Http_1_1,
  )

  val get = Request(
    url = URL(!! / "blobs" / "4073041693a9a66983e6ffb75b521310d30e6db60afc0f97d440cb816bce7c63"),
    method = Method.GET,
    headers = Headers.empty,
    version = Version.Http_1_1,
  )

  def spec = suite("http - blob")(
    test("DELETE -> status") {
      for {
        statusCodeDelete <- app(delete).map(response => response.status.code)
      } yield assertTrue(statusCodeDelete == 404)
    } +
    test("GET -> status") {
      for {
        statusCode <- app(get).map(response => response.status.code)
      } yield assertTrue(statusCode == 404)
    } +
    test("POST / GET") {
      for {
        hash <- app(post).flatMap(h =>
          app(get).flatMap(response =>
            // HashFunction.hashSHA256(response.bodyAsStream).map(bytesToHex)
            response.bodyAsStream
                    .run(ZSink.digest(MessageDigest.getInstance("SHA-256").nn))
                    .map((chunk: Chunk[Byte]) => HexString.bytesToHex(chunk.toArray))
          )
        )
      } yield assertTrue(hash == HexString("4073041693a9a66983e6ffb75b521310d30e6db60afc0f97d440cb816bce7c63"))
    } +
    test("POST / DELETE / GET") {
      for {
        statusCodePost <- app(post).map(response => response.status.code)
        statusCodeDelete <- app(delete).map(response => response.status.code)
        statusCodeGet <- app(get).map(response => response.status.code)
      } yield assertTrue(statusCodePost == 200, statusCodeDelete == 200, statusCodeGet == 404)
    } +
    test("POST -> status") {
      for {
        statusCode <- app(post).map(response => response.status.code)
      } yield assertTrue(statusCode == 200)
    } +
    test("POST -> hash response") {
      for {
        body <- app(post).flatMap(response => response.bodyAsString)
      } yield assertTrue(body == "4073041693a9a66983e6ffb75b521310d30e6db60afc0f97d440cb816bce7c63")
    } +
    test("POST / DELETE") {
      for {
        statusCode <- app(post).flatMap(_ => app(get).map(response => response.status.code))
      } yield assertTrue(statusCode == 200)
    } 

  ).provideCustomLayerShared(environment) @@ 
    TestAspect.sequential @@ 
    TestAspect.before(ZIO.succeed(FileSystem.deleteAllFiles(blobBasePath.toFile().nn)))

