package is.clipperz.backend.apis

import java.nio.charset.StandardCharsets
import java.nio.file.FileSystems
import zio.ZIO
import zio.json.EncoderOps
import zio.test.{ ZIOSpecDefault, assertNever, assertTrue }
import zio.http.{ Version, Headers, Method, URL, Request, Body }
import zio.http.*
import is.clipperz.backend.Main
import is.clipperz.backend.data.HexString
import is.clipperz.backend.data.HexString.bytesToHex
import zio.test.TestAspect
import is.clipperz.backend.functions.fromStream
import is.clipperz.backend.functions.FileSystem
import is.clipperz.backend.services.PRNG
import is.clipperz.backend.services.SessionManager
import is.clipperz.backend.services.UserArchive
import is.clipperz.backend.services.BlobArchive
import is.clipperz.backend.services.TollManager
import is.clipperz.backend.services.SrpManager
import is.clipperz.backend.services.UserCard
import is.clipperz.backend.services.SignupData
import is.clipperz.backend.services.Session
import zio.test.ZIOSpec
import zio.{ Scope, ZLayer, Layer }
import zio.test.TestResult.all
import is.clipperz.backend.services.ModifyUserCard
import is.clipperz.backend.services.OneTimeShareArchive

object UserSpec extends ZIOSpec[SessionManager]:
  override def bootstrap: ZLayer[Any, Any, SessionManager] =
    sessionManagerLayer

  val app = Main.clipperzBackend
  val blobBasePath = FileSystems.getDefault().nn.getPath("target", "tests", "archive", "blobs").nn
  val userBasePath = FileSystems.getDefault().nn.getPath("target", "tests", "archive", "users").nn
  val oneTimeShareBasePath = FileSystems.getDefault().nn.getPath("target", "tests", "archive", "one_time_share").nn

  val sessionManagerLayer: Layer[Nothing, SessionManager] = SessionManager.live

  val environment =
    PRNG.live ++
      sessionManagerLayer ++
      UserArchive.fs(userBasePath, 2, false) ++
      BlobArchive.fs(blobBasePath, 2, false) ++
      OneTimeShareArchive.fs(oneTimeShareBasePath, 2, false) ++
      ((UserArchive.fs(userBasePath, 2, false) ++ PRNG.live) >>> SrpManager.v6a()) ++
      (PRNG.live >>> TollManager.live)

  val sessionKey = "sessionKey"

  val c = HexString("7815018e9d84b5b0f319c87dee46c8876e85806823500e03e72c5d66e5d40456")
  val p = HexString("597ed0c523f50c6db089a92845693a3f2454590026d71d6a9028a69967d33f6d")
  val testUser = UserCard(
    c,
    HexString("0bedc30aca36a67fcc8a7d5d81e72c0dcf10fd20cd8d03f0bbb788807304c3b6"),
    HexString(
      "83fd4a3fc7ec1a37a813a166403ef6b388c5f60e37902b40b4d826ef69f6d88372ba0b9ce777b74e921b9f63e3ddd9e90e0669811fcd8fb4281f8719ec98c244e6dd83ad561a905d908477911f674da4086fe7a9ceadd343f9a930eda9ae29a2ce5bdaca0d2992979f765782d12d0de8ade8745a60395d11ba584abbc08b59d5"
    ),
    "6a",
    "1.0",
    HexString(
      "44457969993591d8c38610c6dec54a7cf66426d7cc5614917727d7d2a5e3a78d356055037b8463cc79a1fe122f55ff5709e5b5fcad8478d183f1b30c2e6acdbb"
    )
  )
  val preferencesContent = HexString(
    "6a641705a78264af4720e561deffb762d069d6d417631cf4bf0ab5a5f006399cb44782fb22d31381c6ac3495f46d2115f45d0ad384d291be0b3e2c9c0c2ae0ab82f149912fd5ece4d267651f64e9c528d04824f1cc5fd0c59fbfb1539eaca7b4f07333b6aa477b7e83680fe65d03713a67a78d4b9b5504fb27bf20dead824a1de646f21019e8f6378163a7fd1114a341ee565a281d37a49e5e79615fd7e0f7b97cdb5f6a5ac96e1c7c45dffc0e19f905bdea44795d27629c8301cc61648e5ebfedcb27c480af3d71c2cc1d84b071bcf669eca79aa677d1c9e3a5b4b0e119015af5648d153c62eaa890c172a6e2f736736d2c9c8b9f4d55995f2bb2bbf1de8a3b799a6d2686d2a21fae774ccca96a442aab9d187347978cfacec3b9b2e6404521cf7a403a4a8e00d6066eb7db41f431ec5aa437e2244bbb7859ad5cb12b94411d9e42e6f66440da039df051e8faf21fc419cfb00c6e2561b4da1cc98fed86e708c93372b6615bfba45f23c5a7c75b6339632b8dbebaea1ea14d4ab94949211da8d07d0af3b7237e652f96a6c00e2bb796c9438518de6a8e00782c1b2616774de2d9cf117af474d7a1d1ea0dd35653274626c9db4fef2a22ab07a39144d89ebe0566ee13ed14147af6f059e1f56365548f648f2d32ebbd5893dc9cd0a1fcf1280d6f50c67a83d164"
  )
  val preferencesReference = HexString("f23cb992df6fe7cbe747ec43dd4d033c086d80608f94ce8929da1c5fc647890f")
  val indexCardContent = HexString(
    "6a641705a78264af4720e561deffb762d069d6d417631cf4bf0ab5a5f006399cb44782fb22d31381c6ac3495f46d2115f45d0ad384d291be0b3e2c9c0c2ae0ab82f149912fd5ece4d267651f64e9c528d04824f1cc5fd0c59fbfb1539eaca7b4f07333b6aa477b7e83680fe65d03713a67a78d4b9b5504fb27bf20dead824a1de646f21019e8f6378163a7fd1114a341ee565a281d37a49e5e79615fd7e0f7b97cdb5f6a5ac96e1c7c45dffc0e19f905bdea44795d27629c8301cc61648e5ebfedcb27c480af3d71c2cc1d84b071bcf669eca79aa677d1c9e3a5b4b0e119015af5648d153c62eaa890c172a6e2f736736d2c9c8b9f4d55995f2bb2bbf1de8a3b799a6d2686d2a21fae774ccca96a442aab9d187347978cfacec3b9b2e6404521cf7a403a4a8e00d6066eb7db41f431ec5aa437e2244bbb7859ad5cb12b94411d9e42e6f66440da039df051e8faf21fc419cfb00c6e2561b4da1cc98fed86e708c93372b6615bfba45f23c5a7c75b6339632b8dbebaea1ea14d4ab94949211da8d07d0af3b7237e652f96a6c00e2bb796c9438518de6a8e00782c1b2616774de2d9cf117af474d7a1d1ea0dd35653274626c9db4fef2a22ab07a39144d89ebe0566ee13ed14147af6f059e1f56365548f648f2d32ebbd5893dc9cd0a1fcf1280d6f50c67a83d164"
  )
  val indexCardReference = HexString("f23cb992df6fe7cbe747ec43dd4d033c086d80608f94ce8929da1c5fc647890f")

  val testSignupData = SignupData(testUser, preferencesReference, preferencesContent, indexCardReference, indexCardContent, Array[(HexString, HexString)]())

  val testUser2 = UserCard(
    c,
    HexString("0bedc30aca36a67fcc8a7d5d81e72c0dcf10fd20cd8d03f0bbb788807304c3b6"),
    HexString(
      "83fd4a3fc7ec1a37a813a166403ef6b388c5f60e37902b40b4d826ef69f6d88372ba0b9ce777b74e921b9f63e3ddd9e90e0669811fcd8fb4281f8719ec98c244e6dd83ad561a905d908477911f674da4086fe7a9ceadd343f9a930eda9ae29a2ce5bdaca0d2992979f765782d12d0de8ade8745a60395d11ba584abbc08b59d5"
    ),
    "6a",
    "2.0",
    HexString(
      "44457969993591d8c38610c6dec54a7cf66426d7cc5614917727d7d2a5e3a78d356055037b8463cc79a1fe122f55ff5709e5b5fcad8478d183f1b30c2e6acdbb"
    )
  )

  val testUserDifferentC = UserCard(
    HexString("aaaaa"),
    HexString("0bedc30aca36a67fcc8a7d5d81e72c0dcf10fd20cd8d03f0bbb788807304c3b6"),
    HexString(
      "83fd4a3fc7ec1a37a813a166403ef6b388c5f60e37902b40b4d826ef69f6d88372ba0b9ce777b74e921b9f63e3ddd9e90e0669811fcd8fb4281f8719ec98c244e6dd83ad561a905d908477911f674da4086fe7a9ceadd343f9a930eda9ae29a2ce5bdaca0d2992979f765782d12d0de8ade8745a60395d11ba584abbc08b59d5"
    ),
    "6a",
    "1.0",
    HexString(
      "44457969993591d8c38610c6dec54a7cf66426d7cc5614917727d7d2a5e3a78d356055037b8463cc79a1fe122f55ff5709e5b5fcad8478d183f1b30c2e6acdbb"
    )
  )

  def preparePost(
      c: String,
      signupData: String,
      session: Boolean,
    ) =
    Request(
      url = URL(!! / "users" / c),
      method = Method.POST,
      headers = if (session) Headers((SessionManager.sessionKeyHeaderName, sessionKey)) else Headers.empty,
      body = Body.fromString(signupData, StandardCharsets.UTF_8.nn),
      version = Version.Http_1_1,
      remoteAddress = None
    )

  def prepareGet(c: String, session: Boolean): Request =
    Request(
      url = URL(!! / "users" / c),
      method = Method.GET,
      headers = if (session) Headers((SessionManager.sessionKeyHeaderName, sessionKey)) else Headers.empty,
      body = Body.empty,
      version = Version.Http_1_1,
      remoteAddress = None
    )

  def prepareDelete(c: String, userData: String, session: Boolean): Request =
    Request(
      url = URL(!! / "users" / c),
      method = Method.DELETE,
      headers = if (session) Headers((SessionManager.sessionKeyHeaderName, sessionKey)) else Headers.empty,
      body = Body.fromString(userData, StandardCharsets.UTF_8.nn),
      version = Version.Http_1_1,
      remoteAddress = None
    )

  def preparePut(c: String, putData: String, session: Boolean): Request =
    Request(
      url = URL(!! / "users" / c),
      method = Method.PUT,
      headers = if (session) Headers((SessionManager.sessionKeyHeaderName, sessionKey)) else Headers.empty,
      body = Body.fromString(putData, StandardCharsets.UTF_8.nn),
      version = Version.Http_1_1,
      remoteAddress = None
    )

  def prepareSession(c: String): ZIO[SessionManager, Throwable, Unit] =
    ZIO
      .service[SessionManager]
      .flatMap(sessionManager => sessionManager.saveSession(Session(sessionKey, Map(("c", c)))))
      .map(_ => ())

  def deleteSession(request: Request): ZIO[SessionManager, Throwable, Unit] =
    ZIO
      .service[SessionManager]
      .flatMap(sessionManager => sessionManager.deleteSession(request))
      .map(_ => ())

  def spec = suite("UserApis")(
    test("GET with no session -> 400") {
      for {
        statusCode <- app.runZIO(prepareGet(c.toString(), true)).map(res => res.status.code)
      } yield assertTrue(statusCode == 400)
    },
    test("DELETE no session -> 400") {
      for {
        statusCode <- app.runZIO(prepareDelete(c.toString(), testUser.toJson, true)).map(res => res.status.code)
      } yield assertTrue(statusCode == 400)
    },
    test("GET non existent user -> 404") {
      for {
        _ <- prepareSession(c.toString())
        statusCode <- app.runZIO(prepareGet(c.toString(), true)).map(res => res.status.code)
      } yield assertTrue(statusCode == 404)
    } @@ TestAspect.after(deleteSession(prepareGet(c.toString(), true)))
      @@ TestAspect.before(ZIO.succeed(FileSystem.deleteAllFiles(userBasePath.toFile().nn))),
    test("GET no session header -> 400") {
      for {
        _ <- prepareSession(c.toString())
        statusCode <- app.runZIO(prepareGet(c.toString(), false)).map(res => res.status.code)
      } yield assertTrue(statusCode == 400)
    } @@ TestAspect.after(deleteSession(prepareGet(c.toString(), true))),
    test("DELETE non existent user -> 404") {
      for {
        _ <- prepareSession(c.toString())
        statusCode <- app.runZIO(prepareDelete(c.toString(), testUser.toJson, true)).map(res => res.status.code)
      } yield assertTrue(statusCode == 404)
    } @@ TestAspect.after(deleteSession(prepareDelete(c.toString(), testUser.toJson, true)))
      @@ TestAspect.before(ZIO.succeed(FileSystem.deleteAllFiles(userBasePath.toFile().nn))),
    test("DELETE no session header -> 400") {
      for {
        _ <- prepareSession(c.toString())
        statusCode <- app.runZIO(prepareDelete(c.toString(), testUser.toJson, false)).map(res => res.status.code)
      } yield assertTrue(statusCode == 400)
    },
    test("POST different c -> 400") {
      for {
        statusCode <- app.runZIO(preparePost("wrongC", testSignupData.toJson, true)).map(res => res.status.code)
      } yield assertTrue(statusCode == 400)
    },
    test("POST wrong data -> 400") {
      for {
        statusCode <- app.runZIO(preparePost("wrongC", "invalidData", true)).map(res => res.status.code)
      } yield assertTrue(statusCode == 400)
    },
    test("POST without session header -> 200") {
      for {
        statusCode <- app.runZIO(preparePost(c.toString(), testSignupData.toJson, false)).map(res => res.status.code)
      } yield assertTrue(statusCode == 200)
    } @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(userBasePath.toFile().nn))),
    test("POST duplicated user -> 409") {
      for {
        _ <- app.runZIO(preparePost(c.toString(), testSignupData.toJson, false)).map(res => res.status.code)
        statusCode <- app.runZIO(preparePost(c.toString(), testSignupData.toJson, false)).map(res => res.status.code)
      } yield assertTrue(statusCode == 409)
    } @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(userBasePath.toFile().nn))),
    test("POST / GET -> 200,200 and content") {
      for {
        postCode <- app.runZIO(preparePost(c.toString(), testSignupData.toJson, false)).map(res => res.status.code)
        _ <- prepareSession(c.toString())
        res <- app.runZIO(prepareGet(c.toString(), true)).flatMap(result =>
          if result.status.code == 200 then
            fromStream[UserCard](result.body.asStream)
              .map(card => assertTrue(result.status.code == 200, card == testUser))
          else ZIO.succeed(assertNever(s"Wrong GET result code: ${result.status.code}"))
        )
      } yield all(assertTrue(postCode == 200), res)
    } @@ TestAspect.after(deleteSession(prepareGet(c.toString(), true)))
      @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(userBasePath.toFile().nn))),
    test("PUT no session -> 400") {
      for {
        res <- app.runZIO(preparePut(c.toString(), ModifyUserCard(c, testUser, testUser2).toJson, true)).map(r => r.status.code)
      } yield assertTrue(res == 400)
    } @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(userBasePath.toFile().nn))),
    test("PUT no session header -> 400") {
      for {
        _ <- prepareSession(c.toString())
        putCode <- app.runZIO(preparePut(c.toString(), ModifyUserCard(c, testUser, testUser2).toJson, false)).map(res => res.status.code)
      } yield assertTrue(putCode == 400)
    } @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(userBasePath.toFile().nn))),
    test("PUT non existent user -> 404") {
      for {
        _ <- prepareSession(c.toString())
        putCode <- app.runZIO(preparePut(c.toString(), ModifyUserCard(c, testUser, testUser2).toJson, true)).map(res => res.status.code)
      } yield assertTrue(putCode == 404)
    } @@ TestAspect.after(deleteSession(preparePut(c.toString(), ModifyUserCard(c, testUser, testUser2).toJson, true)))
      @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(userBasePath.toFile().nn))),
    test("POST / PUT different c -> _, 400") {
      for {
        _ <- app.runZIO(preparePost(c.toString(), testSignupData.toJson, false)).map(res => res.status.code)
        _ <- prepareSession(c.toString())
        putCode <- app.runZIO(preparePut(c.toString(), ModifyUserCard(HexString("ccc"), testUser, testUser2).toJson, true)).map(res => res.status.code)
      } yield assertTrue(putCode == 400)
    } @@ TestAspect.after(deleteSession(preparePut(c.toString(), ModifyUserCard(HexString("ccc"), testUser, testUser2).toJson, true)))
      @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(userBasePath.toFile().nn))),
    test("POST / PUT bad old user -> _, 400") {
      for {
        _ <- app.runZIO(preparePost(c.toString(), testSignupData.toJson, false)).map(res => res.status.code)
        _ <- prepareSession(c.toString())
        putCode <- app.runZIO(preparePut(c.toString(), ModifyUserCard(c, testUserDifferentC, testUser2).toJson, true)).map(res => res.status.code)
      } yield assertTrue(putCode == 400)
    } @@ TestAspect.after(deleteSession(preparePut(c.toString(), ModifyUserCard(c, testUserDifferentC, testUser2).toJson, true)))
      @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(userBasePath.toFile().nn))),
    test("POST / PUT bad data -> _, 400") {
      for {
        _ <- app.runZIO(preparePost(c.toString(), testSignupData.toJson, false)).map(res => res.status.code)
        _ <- prepareSession(c.toString())
        putCode <- app.runZIO(preparePut(c.toString(), "invalidData", true)).map(res => res.status.code)
      } yield assertTrue(putCode == 400)
    } @@ TestAspect.after(deleteSession(preparePut(c.toString(), "invalidData", true)))
      @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(userBasePath.toFile().nn))),
    test("POST / PUT / GET -> _, 200, 200 and content") {
      for {
        _ <- app.runZIO(preparePost(c.toString(), testSignupData.toJson, false))
        _ <- prepareSession(c.toString())
        putCode <- app.runZIO(preparePut(c.toString(), ModifyUserCard(c, testUser, testUser2).toJson, true)).map(res => res.status.code)
        res <- app.runZIO(prepareGet(c.toString(), true)).flatMap(result =>
          if result.status.code == 200 then
            fromStream[UserCard](result.body.asStream)
              .map(card => assertTrue(result.status.code == 200, card == testUser2))
          else ZIO.succeed(assertNever(s"Wrong GET result code: ${result.status.code}"))
        )
      } yield all(assertTrue(putCode == 200), res)
    } @@ TestAspect.after(deleteSession(prepareGet(c.toString(), true)))
      @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(userBasePath.toFile().nn))),
    test("POST / DELETE -> _, 200") {
      for {
        _ <- app.runZIO(preparePost(c.toString(), testSignupData.toJson, false))
        _ <- prepareSession(c.toString())
        deleteCode <- app.runZIO(prepareDelete(c.toString(), testUser.toJson, true)).map(res => res.status.code)
      } yield assertTrue(deleteCode == 200)
    } @@ TestAspect.after(deleteSession(prepareDelete(c.toString(), testUser.toJson, true)))
      @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(userBasePath.toFile().nn))),
    test("POST / DELETE / GET -> _, 200, 400") {
      for {
        _ <- app.runZIO(preparePost(c.toString(), testSignupData.toJson, false))
        _ <- prepareSession(c.toString())
        deleteCode <- app.runZIO(prepareDelete(c.toString(), testUser.toJson, true)).map(res => res.status.code)
        getCode <- app.runZIO(prepareGet(c.toString(), true)).map(res => res.status.code)
      } yield assertTrue(deleteCode == 200, getCode == 400)
    } @@ TestAspect.after(deleteSession(prepareGet(c.toString(), true)))
      @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(userBasePath.toFile().nn))),
    test("POST / DELETE / GET -> _, 200, 404") {
      for {
        _ <- app.runZIO(preparePost(c.toString(), testSignupData.toJson, false))
        _ <- prepareSession(c.toString())
        deleteCode <- app.runZIO(prepareDelete(c.toString(), testUser.toJson, true)).map(res => res.status.code)
        _ <- prepareSession(c.toString())
        getCode <- app.runZIO(prepareGet(c.toString(), true)).map(res => res.status.code)
      } yield assertTrue(deleteCode == 200, getCode == 404)
    } @@ TestAspect.after(deleteSession(prepareGet(c.toString(), true)))
      @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(userBasePath.toFile().nn))),
  ).provideLayerShared(environment)
    @@ TestAspect.sequential
    @@ TestAspect.afterAll(ZIO.succeed(FileSystem.deleteAllFiles(blobBasePath.toFile().nn)))
    @@ TestAspect.afterAll(ZIO.succeed(FileSystem.deleteAllFiles(userBasePath.toFile().nn)))
    // @@ TestAspect.after(deleteSession())
    // @@ TestAspect.before(ZIO.succeed(FileSystem.deleteAllFiles(userBasePath.toFile().nn))),