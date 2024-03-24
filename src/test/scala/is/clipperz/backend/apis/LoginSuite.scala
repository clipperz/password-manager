package is.clipperz.backend.apis

import java.io.File
// import java.nio.charset.StandardCharsets
// import java.nio.file.{ Files, Paths, FileSystems }
import java.security.MessageDigest
import scala.language.postfixOps
import zio.{ Chunk, ZIO, Task }
import zio.nio.file.{ FileSystem }
import zio.nio.charset.Charset
import zio.stream.{ ZStream, ZSink }
import zio.test.Assertion.{ nothing, isTrue }
import zio.test.{ ZIOSpecDefault, assertZIO, assertNever, assertTrue, assert, TestAspect }
import zio.json.EncoderOps
import zio.http.{ Version, Headers, Method, URL, Request, Body }
import zio.http.*
import is.clipperz.backend.Main
import is.clipperz.backend.data.HexString
import is.clipperz.backend.data.HexString.bytesToHex
import is.clipperz.backend.functions.crypto.HashFunction
// import java.nio.file.Path
// import is.clipperz.backend.functions.FileSystem
import is.clipperz.backend.services.PRNG
import is.clipperz.backend.services.SessionManager
import is.clipperz.backend.services.UserArchive
import is.clipperz.backend.services.BlobArchive
import is.clipperz.backend.services.TollManager
import is.clipperz.backend.services.SrpManager
import is.clipperz.backend.functions.Conversions.{ bytesToBigInt, bigIntToBytes }
import is.clipperz.backend.functions.fromStream
import is.clipperz.backend.services.UserCard
import java.sql.Blob
import zio.test.ZIOSpec
import zio.Scope
import zio.ZLayer
import is.clipperz.backend.services.SRPStep1Data
import is.clipperz.backend.data.srp.RFCTestVector
import is.clipperz.backend.services.SRPStep1Response
import is.clipperz.backend.services.SRPStep2Data
import is.clipperz.backend.functions.SrpFunctions.SrpFunctionsV6a
import is.clipperz.backend.functions.SrpFunctions
import is.clipperz.backend.services.SRPStep2Response
import is.clipperz.backend.services.OneTimeShareArchive
import is.clipperz.backend.services.RequestUserCard
import is.clipperz.backend.services.RemoteUserCard
import is.clipperz.backend.functions.customErrorHandler
import is.clipperz.backend.services.SRPVersion
import is.clipperz.backend.services.MasterKeyEncodingVersion
import is.clipperz.backend.TestUtilities

object LoginSpec extends ZIOSpec[UserArchive & BlobArchive]:
    val keyBlobArchiveFolderDepth = 16

    override def bootstrap: ZLayer[Any, Any, UserArchive & BlobArchive] =
        UserArchive.fs(userBasePath, keyBlobArchiveFolderDepth, false) ++
        BlobArchive.fs(blobBasePath, keyBlobArchiveFolderDepth, false)

    val app =   loginApi
                .handleErrorCauseZIO(customErrorHandler)
                .toHttpApp
    val blobBasePath            = FileSystem.default.getPath("target", "tests", "archive", "blobs")
    val userBasePath            = FileSystem.default.getPath("target", "tests", "archive", "users")
    val oneTimeShareBasePath    = FileSystem.default.getPath("target", "tests", "archive", "one_time_share")

    val environment =
        PRNG.live ++
        (PRNG.live >>> SessionManager.live()) ++
        UserArchive.fs(userBasePath, keyBlobArchiveFolderDepth, false) ++
        BlobArchive.fs(blobBasePath, keyBlobArchiveFolderDepth, false) ++
        OneTimeShareArchive.fs(oneTimeShareBasePath, keyBlobArchiveFolderDepth, false) ++
        ((UserArchive.fs(userBasePath, keyBlobArchiveFolderDepth, false) ++ PRNG.live) >>> SrpManager.v6a()) ++
        (PRNG.live >>> TollManager.live)

    val c = HexString("7815018e9d84b5b0f319c87dee46c8876e85806823500e03e72c5d66e5d40456")
    val p = HexString("597ed0c523f50c6db089a92845693a3f2454590026d71d6a9028a69967d33f6d")
    val testUser = RemoteUserCard(
        c,
        HexString("0bedc30aca36a67fcc8a7d5d81e72c0dcf10fd20cd8d03f0bbb788807304c3b6"),
        HexString("83fd4a3fc7ec1a37a813a166403ef6b388c5f60e37902b40b4d826ef69f6d88372ba0b9ce777b74e921b9f63e3ddd9e90e0669811fcd8fb4281f8719ec98c244e6dd83ad561a905d908477911f674da4086fe7a9ceadd343f9a930eda9ae29a2ce5bdaca0d2992979f765782d12d0de8ade8745a60395d11ba584abbc08b59d5"),
        SRPVersion("6a"),
        (
            HexString("44457969993591d8c38610c6dec54a7cf66426d7cc5614917727d7d2a5e3a78d356055037b8463cc79a1fe122f55ff5709e5b5fcad8478d183f1b30c2e6acdbb"),
            MasterKeyEncodingVersion("1.0")
        ),
    )
    val indexCardContent    = "6a641705a78264af4720e561deffb762d069d6d417631cf4bf0ab5a5f006399cb44782fb22d31381c6ac3495f46d2115f45d0ad384d291be0b3e2c9c0c2ae0ab82f149912fd5ece4d267651f64e9c528d04824f1cc5fd0c59fbfb1539eaca7b4f07333b6aa477b7e83680fe65d03713a67a78d4b9b5504fb27bf20dead824a1de646f21019e8f6378163a7fd1114a341ee565a281d37a49e5e79615fd7e0f7b97cdb5f6a5ac96e1c7c45dffc0e19f905bdea44795d27629c8301cc61648e5ebfedcb27c480af3d71c2cc1d84b071bcf669eca79aa677d1c9e3a5b4b0e119015af5648d153c62eaa890c172a6e2f736736d2c9c8b9f4d55995f2bb2bbf1de8a3b799a6d2686d2a21fae774ccca96a442aab9d187347978cfacec3b9b2e6404521cf7a403a4a8e00d6066eb7db41f431ec5aa437e2244bbb7859ad5cb12b94411d9e42e6f66440da039df051e8faf21fc419cfb00c6e2561b4da1cc98fed86e708c93372b6615bfba45f23c5a7c75b6339632b8dbebaea1ea14d4ab94949211da8d07d0af3b7237e652f96a6c00e2bb796c9438518de6a8e00782c1b2616774de2d9cf117af474d7a1d1ea0dd35653274626c9db4fef2a22ab07a39144d89ebe0566ee13ed14147af6f059e1f56365548f648f2d32ebbd5893dc9cd0a1fcf1280d6f50c67a83d164"
    val indexCardReference  = "f23cb992df6fe7cbe747ec43dd4d033c086d80608f94ce8929da1c5fc647890f"

    val identifier = HexString("abba")

    val saveUser: ZIO[UserArchive & BlobArchive, Throwable, Unit] =
        ZIO
        .service[UserArchive]
        .zip(ZIO.service[BlobArchive])
        .flatMap((userArchive, blobArchive) =>
            userArchive
            .saveUser(testUser, false)
            .flatMap(_ =>
                blobArchive
                .saveBlob(HexString(indexCardReference), identifier, ZStream.fromIterable(HexString(indexCardContent).toByteArray))
                .map(_ => ())
            )
        )
        .catchAll(e => ZIO.succeed(()))

    val sessionKey = "sessionKey"

    def loginRequestStep1 (c: String, stepData: String, withSession: Boolean): Task[Request] =
        Charset.Standard.utf8.encodeString(stepData)
        .map(body =>
            Request(
                url = URL(Root / "api" / "login" / "step1" / c),
                method = Method.POST,
                headers = if (withSession) Headers((SessionManager.sessionKeyHeaderName, sessionKey)) else Headers.empty,
                body = Body.fromChunk(body),
                version = Version.Http_1_1,
                remoteAddress = None
            )
        )

    def loginRequestStep2 (c: String, stepData: String, withSession: Boolean): Task[Request] =
        Charset.Standard.utf8.encodeString(stepData)
        .map(body =>
            Request(
                url = URL(Root / "api" / "login" / "step2" / c),
                method = Method.POST,
                headers = if (withSession) Headers((SessionManager.sessionKeyHeaderName, sessionKey)) else Headers.empty,
                body = Body.fromChunk(body),
                version = Version.Http_1_1,
                remoteAddress = None
            )
        )

    def spec = suite("LoginApis")(
        test("Login step 1 - fail - user not found") {
            val stepData = SRPStep1Data(HexString("aaa"), HexString("aaa"))
            // val request = loginRequestStep1("aaa", stepData.toJson, true)
            // assertZIO(app.runZIO(request).map(response => response.status.code == 404))(isTrue)
            assertZIO(
                loginRequestStep1("aaa", stepData.toJson, true)
                .flatMap(request => app.runZIO(request).map(response => response.status.code == 404))
            )(isTrue)
        },
        test("Login step 1 - fail - wrong c") {
            val stepData = SRPStep1Data(HexString("aaa"), HexString("aaa"))
            // val request = loginRequestStep1("ccc", stepData.toJson, true)
            // assertZIO(app.runZIO(request).map(response => response.status.code == 400))(isTrue)
            assertZIO(
                loginRequestStep1("ccc", stepData.toJson, true)
                .flatMap( request => app.runZIO(request).map(response => response.status.code == 400))
            )(isTrue)
        },
        test("Login step 1 - fail - invalid data") {
            // val request = loginRequestStep1("ccc", "invalid", true)
            // assertZIO(app.runZIO(request).map(response => response.status.code == 400))(isTrue)
            assertZIO(
                loginRequestStep1("ccc", "invalid", true)
                .flatMap(request => app.runZIO(request).map(response => response.status.code == 400))
            )(isTrue)
        },
        test("Login step 1 - fail - no sessionKey") {
            // val request = loginRequestStep1("ccc", "invalid", false)
            // assertZIO(app.runZIO(request).map(response => response.status.code == 400))(isTrue)
            assertZIO(
                loginRequestStep1("ccc", "invalid", false)
                .flatMap(request => app.runZIO(request).map(response => response.status.code == 400))
            )(isTrue)
        },
        test("Login step 1 - success") {
            val stepData = SRPStep1Data(c, HexString.bigIntToHex(RFCTestVector.aa))
            // val request = loginRequestStep1(c.toString(), stepData.toJson, true)
            for {
                request         <-  loginRequestStep1(c.toString(), stepData.toJson, true)
                response        <-  app.runZIO(request)
                stepResponse    <-  fromStream[SRPStep1Response](response.body.asStream)
            } yield assertTrue(response.status.code == 200, stepResponse.s == testUser.s)
        } @@ TestAspect.before(saveUser),
        test("Login step 1, 2 - fail - invalid data") {
            val aa = RFCTestVector.aa
            val stepData = SRPStep1Data(c, HexString.bigIntToHex(aa))
            // val request = loginRequestStep1(c.toString(), stepData.toJson, true)
            for {
                request         <-  loginRequestStep1(c.toString(), stepData.toJson, true)
                srpManager      <-  ZIO.service[SrpManager]
                response        <-  app.runZIO(request)
                stepResponse    <-  fromStream[SRPStep1Response](response.body.asStream)
                request2        <-  loginRequestStep2(c.toString(), "invalid", true)
                response2       <-  app.runZIO(request2)
            } yield assertTrue(response2.status.code == 400)
        },
        test("Login step 1, 2 - fail - wrong m1") {
            val aa = RFCTestVector.aa
            val stepData = SRPStep1Data(c, HexString.bigIntToHex(aa))
            // val request = loginRequestStep1(c.toString(), stepData.toJson, true)
            val m1 = HexString("abcdef")
            for {
                request         <-  loginRequestStep1(c.toString(), stepData.toJson, true)
                srpManager      <-  ZIO.service[SrpManager]
                response        <-  app.runZIO(request)
                stepResponse    <-  fromStream[SRPStep1Response](response.body.asStream)
                request2        <-  loginRequestStep2(c.toString(), SRPStep2Data(m1).toJson, true)
                response2       <-  app.runZIO(request2)
            } yield assertTrue(response2.status.code == 400)
        },
        test("Login step 1, 2 - fail - no sessionKey") {
            val aa = RFCTestVector.aa
            val a = RFCTestVector.a
            val stepData = SRPStep1Data(c, HexString.bigIntToHex(aa))
            // val request = loginRequestStep1(c.toString(), stepData.toJson, true)
            val srpFunctions = new SrpFunctionsV6a()
            for {
                request         <-  loginRequestStep1(c.toString(), stepData.toJson, true)
                srpManager      <-  ZIO.service[SrpManager]
                response        <-  app.runZIO(request)
                stepResponse    <-  fromStream[SRPStep1Response](response.body.asStream)

                u               <-  srpFunctions.computeU(bigIntToBytes(aa), stepResponse.bb.toByteArray)
                config          <-  ZIO.succeed(srpFunctions.configuration)
                x               <-  config.keyDerivationFunction(stepResponse.s.toByteArray, p.toByteArray).map(bytes => bytesToBigInt(bytes))
                clientSecret    <-  ZIO.succeed(srpFunctions.computeSecretClient(stepResponse.bb.toBigInt, x, a, bytesToBigInt(u)))
                kk              <-  srpFunctions.configuration.hash(ZStream.fromIterable(bigIntToBytes(clientSecret)))

                m1              <-  srpFunctions.computeM1(
                                        c.toByteArray,
                                        stepResponse.s.toByteArray,
                                        stepData.aa.toByteArray,
                                        stepResponse.bb.toByteArray,
                                        kk,
                                    )
                request2        <-  loginRequestStep2(c.toString(), SRPStep2Data(HexString.bytesToHex(m1)).toJson, false)
                response2       <-  app.runZIO(request2)
            } yield assertTrue(response2.status.code == 400)
        },
        test("Login step 1, 2 - success") {
            val aa = RFCTestVector.aa
            val a = RFCTestVector.a
            val stepData = SRPStep1Data(c, HexString.bigIntToHex(aa))
            // val request = loginRequestStep1(c.toString(), stepData.toJson, true)
            val srpFunctions = new SrpFunctionsV6a()
            for {
                request         <-  loginRequestStep1(c.toString(), stepData.toJson, true)
                srpManager      <-  ZIO.service[SrpManager]
                response        <-  app.runZIO(request)
                stepResponse    <-  fromStream[SRPStep1Response](response.body.asStream)

                u               <-  srpFunctions.computeU(bigIntToBytes(aa), stepResponse.bb.toByteArray)
                config          <-  ZIO.succeed(srpFunctions.configuration)
                x               <-  config.keyDerivationFunction(stepResponse.s.toByteArray, p.toByteArray).map(bytes => bytesToBigInt(bytes))
                clientSecret    <-  ZIO.succeed(srpFunctions.computeSecretClient(stepResponse.bb.toBigInt, x, a, bytesToBigInt(u)))
                kk              <-  srpFunctions.configuration.hash(ZStream.fromIterable(bigIntToBytes(clientSecret)))

                m1              <-  srpFunctions.computeM1(
                                        c.toByteArray,
                                        stepResponse.s.toByteArray,
                                        stepData.aa.toByteArray,
                                        stepResponse.bb.toByteArray,
                                        kk,
                                    )
                request2        <-  loginRequestStep2(c.toString(), SRPStep2Data(HexString.bytesToHex(m1)).toJson, true)
                response2       <-  app.runZIO(request2)
                stepResponse <- fromStream[SRPStep2Response](response2.body.asStream)
            } yield assertTrue(response2.status.code == 200, stepResponse.masterKey._1 == testUser.masterKey._1)
        },
    ).provideLayerShared(environment) @@
        TestAspect.sequential @@
        TestAspect.afterAll(TestUtilities.deleteFilesInFolder(blobBasePath))
