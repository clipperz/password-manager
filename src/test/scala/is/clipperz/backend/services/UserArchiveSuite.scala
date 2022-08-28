package is.clipperz.backend.services

import java.nio.charset.StandardCharsets
import java.nio.file.FileSystems
import zio.ZIO
import zio.json.{ EncoderOps }
import zio.test.{ ZIOSpecDefault, assertTrue }
import zhttp.http.{ Version, Headers, Method, URL, Request, HttpData }
import zhttp.http.*
import is.clipperz.backend.Main
import is.clipperz.backend.data.HexString
import is.clipperz.backend.data.HexString.{ bytesToHex }

object UserSpec extends ZIOSpecDefault:
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

  def preparePut (c: HexString)=     
    val testUser = UserCard(c, HexString("s"), HexString("v"), "srpVersion_test", "masterKeyEncodingVersion_test", HexString("masterKeyContent_test"))
    val testRequest = SignupData(testUser, 
                                 HexString("af22025a81131eca3c315f2ef038ab2234a54730910a48530ce3f8d71e0ed718"), 
                                 HexString("34f233155174be0c7cde653552919d4a6b37483830550c5542c6d3e626fb66b5514e93f4343997666c3638f52738e9"),
                                 Array[(HexString, HexString)]())
    Request(
      url = URL(!! / "users" / c.toString),
      method = Method.PUT,
      headers = Headers.empty,
      data = HttpData.fromString(testRequest.toJson, StandardCharsets.UTF_8.nn),
      version = Version.Http_1_1,
    )

  def prepareGet (c: HexString): Request =
    Request(
      url = URL(!! / "users" / c.toString),
      method = Method.GET,
      headers = Headers.empty,
      version = Version.Http_1_1,
    )

  def spec = suite("http - user")(
    test("PUT -> status") {
      for {
        prng <- ZIO.service[PRNG]
        c <- prng.nextBytes(64).map(bytesToHex(_))
        statusCode <- app(preparePut(c)).map(response => response.status.code)
      } yield assertTrue(statusCode == 200)
    } +
    test("PUT -> hash response") {
      for {
        prng <- ZIO.service[PRNG]
        c <- prng.nextBytes(64).map(bytesToHex(_))
        body <- app(preparePut(c)).flatMap(response => response.bodyAsString)
      } yield assertTrue(HexString(body) == c)
    } +
    test("UserArchive doesn't overwrite old users when creating a new user") {
      for {
        prng <- ZIO.service[PRNG]
        c <- prng.nextBytes(64).map(bytesToHex(_))
        _ <- app(preparePut(c))
        statusCode <- app(preparePut(c)).map(response => response.status.code)
      } yield assertTrue(statusCode == 409)
    } +
    test("UserArchive overwrites user cards when requested") {
      // TODO: to update the implementation when the function to update the user cards will be implemented
      for {
        prng <- ZIO.service[PRNG]
        card <- prng.nextBytes(64).map(bytesToHex(_)).map(UserCard(_, HexString("s"), HexString("v"), "srpVersion_test", "masterKeyEncodingVersion_test", HexString("masterKeyContent_test")))
        userArchive <- ZIO.service[UserArchive]
        _ <- userArchive.saveUser(card, false)
        c <- userArchive.saveUser(card, true)
      } yield assertTrue(c == card.c) 
    }

  ).provideCustomLayerShared(environment)
