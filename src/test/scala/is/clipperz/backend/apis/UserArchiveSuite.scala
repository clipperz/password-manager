package is.clipperz.backend.apis

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
import zio.test.TestAspect
import is.clipperz.backend.functions.FileSystem
import is.clipperz.backend.services.PRNG
import is.clipperz.backend.services.SessionManager
import is.clipperz.backend.services.UserArchive
import is.clipperz.backend.services.BlobArchive
import is.clipperz.backend.services.TollManager
import is.clipperz.backend.services.SrpManager
import is.clipperz.backend.services.UserCard
import is.clipperz.backend.services.SignupData

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

  val sessionKey = "sessionKey"

  def preparePost (c: HexString)=     
    val testUser = UserCard(c, HexString("s"), HexString("v"), "srpVersion_test", "masterKeyEncodingVersion_test", HexString("masterKeyContent_test"))
    val testRequest = SignupData(testUser, 
                                 HexString("af22025a81131eca3c315f2ef038ab2234a54730910a48530ce3f8d71e0ed718"), 
                                 HexString("34f233155174be0c7cde653552919d4a6b37483830550c5542c6d3e626fb66b5514e93f4343997666c3638f52738e9"),
                                 Array[(HexString, HexString)]())
    Request(
      url = URL(!! / "users" / c.toString),
      method = Method.POST,
      headers = Headers((SessionManager.sessionKeyHeaderName, sessionKey)),
      data = HttpData.fromString(testRequest.toJson, StandardCharsets.UTF_8.nn),
      version = Version.Http_1_1,
    )

  def prepareGet (c: HexString): Request =
    Request(
      url = URL(!! / "users" / c.toString),
      method = Method.GET,
      headers = Headers((SessionManager.sessionKeyHeaderName, sessionKey)),
      version = Version.Http_1_1,
    )

  def preparePut (card: UserCard)=     
    val testRequest = SignupData(card, 
                                 HexString("af22025a81131eca3c315f2ef038ab2234a54730910a48530ce3f8d71e0ed718"), 
                                 HexString("34f233155174be0c7cde653552919d4a6b37483830550c5542c6d3e626fb66b5514e93f4343997666c3638f52738e9"),
                                 Array[(HexString, HexString)]())
    Request(
      url = URL(!! / "users" / card.c.toString),
      method = Method.PUT,
      headers = Headers((SessionManager.sessionKeyHeaderName, sessionKey)),
      data = HttpData.fromString(testRequest.toJson, StandardCharsets.UTF_8.nn),
      version = Version.Http_1_1,
    )

  def preparePostWithCard (card: UserCard)=     
    val testRequest = SignupData(card, 
                                 HexString("af22025a81131eca3c315f2ef038ab2234a54730910a48530ce3f8d71e0ed718"), 
                                 HexString("34f233155174be0c7cde653552919d4a6b37483830550c5542c6d3e626fb66b5514e93f4343997666c3638f52738e9"),
                                 Array[(HexString, HexString)]())
    Request(
      url = URL(!! / "users" / card.c.toString),
      method = Method.POST,
      headers = Headers((SessionManager.sessionKeyHeaderName, sessionKey)),
      data = HttpData.fromString(testRequest.toJson, StandardCharsets.UTF_8.nn),
      version = Version.Http_1_1,
    )

  def spec = suite("http - user")(
    test("PUT -> status") {
      for {
        prng <- ZIO.service[PRNG]
        c <- prng.nextBytes(64).map(bytesToHex(_))
        statusCode <- app(preparePost(c)).map(response => response.status.code)
      } yield assertTrue(statusCode == 200)
    } +
    test("PUT -> hash response") {
      for {
        prng <- ZIO.service[PRNG]
        c <- prng.nextBytes(64).map(bytesToHex(_))
        body <- app(preparePost(c)).flatMap(response => response.bodyAsString)
      } yield assertTrue(HexString(body) == c)
    } +
    test("UserArchive doesn't overwrite old users when creating a new user") {
      for {
        prng <- ZIO.service[PRNG]
        c <- prng.nextBytes(64).map(bytesToHex(_))
        _ <- app(preparePost(c))
        statusCode <- app(preparePost(c)).map(response => response.status.code)
      } yield assertTrue(statusCode == 409)
    } +
    test("UserArchive overwrites user cards when requested") {
      // TODO: to update the implementation when the function to update the user cards will be implemented
      for {
        prng <- ZIO.service[PRNG]
        card <- prng.nextBytes(64).map(bytesToHex(_)).map(UserCard(_, HexString("s"), HexString("v"), "srpVersion_test", "masterKeyEncodingVersion_test", HexString("masterKeyContent_test")))
        _ <- app(preparePostWithCard(card))
        firstSavedCardStr <- app(prepareGet(card.c)).flatMap(_.bodyAsString)
        newCard <- ZIO.succeed(UserCard(card.c, HexString("st"), HexString("v"), "srpVersion_test", "masterKeyEncodingVersion_test", HexString("masterKeyContent_test")))
        _ <- app(preparePut(newCard))
        savedCard <- app(prepareGet(card.c))
        savedCardStr <- savedCard.bodyAsString
      } yield assertTrue(savedCardStr == firstSavedCardStr) 
    }

  ).provideCustomLayerShared(environment) @@ 
    TestAspect.sequential @@ 
    TestAspect.before(ZIO.succeed(FileSystem.deleteAllFiles(blobBasePath.toFile().nn)))
