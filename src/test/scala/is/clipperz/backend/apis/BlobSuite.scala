package is.clipperz.backend.apis

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Paths, FileSystems }
import java.security.MessageDigest
import scala.language.postfixOps
import zio.{ Chunk, ZIO }
import zio.stream.{ ZStream, ZSink }
import zio.test.Assertion.nothing
import zio.test.{ ZIOSpecDefault, assertTrue, assert, TestAspect }
import zio.json.EncoderOps
import zhttp.http.{ Version, Headers, Method, URL, Request, HttpData }
import zhttp.http.*
import is.clipperz.backend.Main
import is.clipperz.backend.data.HexString
import is.clipperz.backend.data.HexString.bytesToHex
import is.clipperz.backend.functions.crypto.HashFunction
import java.nio.file.Path
import is.clipperz.backend.functions.FileSystem
import is.clipperz.backend.services.SaveBlobData
import is.clipperz.backend.services.PRNG
import is.clipperz.backend.services.SessionManager
import is.clipperz.backend.services.UserArchive
import is.clipperz.backend.services.BlobArchive
import is.clipperz.backend.services.TollManager
import is.clipperz.backend.services.SrpManager

import is.clipperz.backend.functions.fromStream

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

  val blobData: SaveBlobData = SaveBlobData(
    data = bytesToHex(
      Files
        .readAllBytes(Paths.get("src/test/resources/blobs/4073041693a9a66983e6ffb75b521310d30e6db60afc0f97d440cb816bce7c63.blob"))
        .nn
    ),
    hash = HexString("4073041693a9a66983e6ffb75b521310d30e6db60afc0f97d440cb816bce7c63"),
  )

  val post = Request(
    url = URL(!! / "blobs"),
    method = Method.POST,
    headers = Headers.empty,
    data = HttpData.fromString(blobData.toJson, StandardCharsets.UTF_8.nn),
    version = Version.Http_1_1,
  )

  val delete = Request(
    url = URL(!! / "blobs" / blobData.hash.toString()),
    method = Method.DELETE,
    headers = Headers.empty,
    data = HttpData.fromString(blobData.toJson, StandardCharsets.UTF_8.nn),
    version = Version.Http_1_1,
  )

  val invalidDelete = Request(
    url = URL(!! / "blobs" / blobData.hash.toString()),
    method = Method.DELETE,
    headers = Headers.empty,
    data = HttpData.fromString("invalidData", StandardCharsets.UTF_8.nn),
    version = Version.Http_1_1,
  )

  val get = Request(
    url = URL(!! / "blobs" / blobData.hash.toString()),
    method = Method.GET,
    headers = Headers.empty,
    version = Version.Http_1_1,
  )

  val invalidBlobKey = "f1234d75178d892a133a410355a5a990cf75d2f33eba25d575943d4df632f3a4"
  val invalidBlobContent = "invalid"

  val postInvalid = Request(
    url = URL(!! / "blobs"),
    method = Method.POST,
    headers = Headers.empty,
    data = HttpData.fromString(invalidBlobContent, StandardCharsets.UTF_8.nn),
    version = Version.Http_1_1,
  )

  val postEmpty = Request(
    url = URL(!! / "blobs"),
    method = Method.POST,
    headers = Headers.empty,
    data = HttpData.fromString("", StandardCharsets.UTF_8.nn),
    version = Version.Http_1_1,
  )

  def spec = suite("BlobApis")(
    test("DELETE valid blob -> 404") {
      for {
        statusCodeDelete <- app(delete).map(response => response.status.code)
      } yield assertTrue(statusCodeDelete == 404)
    },
    test("DELETE invalid blob -> 400") {
      for {
        statusCodeDelete <- app(invalidDelete).map(response => response.status.code)
      } yield assertTrue(statusCodeDelete == 400)
    },
    test("GET non existent blob -> 404") {
      for {
        statusCode <- app(get).map(response => response.status.code)
      } yield assertTrue(statusCode == 404)
    },
    test("POST invalid blob -> 400") {
      for {
        statusCode <- app(postInvalid).map(response => response.status.code)
      } yield assertTrue(statusCode == 400)
    },
    test("POST empty blob -> 400") {
      for {
        statusCode <- app(postEmpty).map(response => response.status.code)
      } yield assertTrue(statusCode == 400)
    },
    test("POST correct blob -> 200") {
      for {
        statusCode <- app(post).map(response => response.status.code)
      } yield assertTrue(statusCode == 200)
    },
    test("POST -> hash response") {
      for {
        body <- app(post).flatMap(response => response.bodyAsString)
      } yield assertTrue(body == blobData.hash.toString())
    },
    test("POST / GET -> 200") {
      for {
        postReseponse <- app(post)
        code <- app(get).map(_.status.code)
      } yield assertTrue(code == 200)
    },
    test("POST / GET -> response content") {
      for {
        postReseponse <- app(post)
        hash <- app(get).flatMap(response =>
          response
            .bodyAsStream
            .run(ZSink.digest(MessageDigest.getInstance("SHA-256").nn))
            .map((chunk: Chunk[Byte]) => HexString.bytesToHex(chunk.toArray))
        )
      } yield assertTrue(hash == blobData.hash)
    },
    test("POST / DELETE -> _, 200") {
      for {
        statusCode <- app(post).flatMap(_ => app(get).map(response => response.status.code))
      } yield assertTrue(statusCode == 200)
    },
    test("POST / DELETE / GET -> 200, 200, 404") {
      for {
        statusCodePost <- app(post).map(response => response.status.code)
        statusCodeDelete <- app(delete).map(response => response.status.code)
        statusCodeGet <- app(get).map(response => response.status.code)
      } yield assertTrue(statusCodePost == 200, statusCodeDelete == 200, statusCodeGet == 404)
    },
  ).provideCustomLayerShared(environment) @@
    TestAspect.sequential @@
    TestAspect.beforeAll(ZIO.succeed(FileSystem.deleteAllFiles(blobBasePath.toFile().nn)))
  TestAspect.afterAll(ZIO.succeed(FileSystem.deleteAllFiles(blobBasePath.toFile().nn)))
