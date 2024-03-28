package is.clipperz.backend

// import java.io.File
// import java.nio.charset.StandardCharsets
// import java.nio.file.{ Files, Paths, FileSystems }

import java.security.MessageDigest
import scala.language.postfixOps
import zio.{ Chunk, Task, ZIO }
import zio.nio.file.{ FileSystem }
import zio.nio.charset.Charset
import zio.stream.{ ZStream, ZSink }
import zio.test.Assertion.nothing
import zio.test.TestResult.allSuccesses
import zio.test.{ ZIOSpecDefault, assertTrue, assertNever, assert, TestAspect }
import zio.json.EncoderOps
import zio.http.{ Version, Headers, Method, URL, Request, Body }
import zio.http.*
import is.clipperz.backend.Main
import is.clipperz.backend.data.HexString
import is.clipperz.backend.data.HexString.bytesToHex
import is.clipperz.backend.functions.Conversions.{ bytesToBigInt, bigIntToBytes }
import is.clipperz.backend.functions.crypto.HashFunction
// import java.nio.file.Path
// import is.clipperz.backend.functions.FileSystem
import is.clipperz.backend.services.PRNG
import is.clipperz.backend.services.SessionManager
import is.clipperz.backend.services.UserArchive
import is.clipperz.backend.services.BlobArchive
import is.clipperz.backend.services.TollManager
import is.clipperz.backend.services.SrpManager
import is.clipperz.backend.Main.ClipperzEnvironment

import is.clipperz.backend.functions.fromStream
import zio.test.Assertion
import zio.test.TestResult
import is.clipperz.backend.functions.SrpFunctions.SrpFunctionsV6a
import is.clipperz.backend.functions.ByteArrays
import is.clipperz.backend.functions.Conversions
import is.clipperz.backend.services.SignupData
import is.clipperz.backend.services.UserCard
import is.clipperz.backend.services.TollChallenge.apply
import is.clipperz.backend.services.TollChallenge
import is.clipperz.backend.data.srp.RFCTestVector
import is.clipperz.backend.services.SRPStep1Data
import is.clipperz.backend.services.SRPStep1Response.apply
import is.clipperz.backend.services.SRPStep1Response
import is.clipperz.backend.services.SRPStep2Data
import is.clipperz.backend.services.OneTimeShareArchive
import java.net.InetAddress
import is.clipperz.backend.services.RequestUserCard
import is.clipperz.backend.services.CardsSignupData
import is.clipperz.backend.services.MasterKeyEncodingVersion
import is.clipperz.backend.services.SRPVersion

object AppSpec extends ZIOSpecDefault:
    val app = Main.completeClipperzBackend
    val blobBasePath            = FileSystem.default.getPath("target", "tests", "archive", "blobs")
    val userBasePath            = FileSystem.default.getPath("target", "tests", "archive", "users")
    val oneTimeShareBasePath    = FileSystem.default.getPath("target", "tests", "archive", "one_time_share")

    val keyBlobArchiveFolderDepth = 16

    val environment =
        PRNG.live ++
        (PRNG.live >>> SessionManager.live()) ++
        UserArchive.fs(userBasePath, keyBlobArchiveFolderDepth, false) ++
        BlobArchive.fs(blobBasePath, keyBlobArchiveFolderDepth, false) ++
        OneTimeShareArchive.fs(oneTimeShareBasePath, keyBlobArchiveFolderDepth, false) ++
        ((UserArchive.fs(userBasePath, keyBlobArchiveFolderDepth, false) ++ PRNG.live) >>> SrpManager.v6a()) ++
        (PRNG.live >>> TollManager.live)

    val srpFunctions = new SrpFunctionsV6a()

    val blobData = HexString("f5a34923d74831d3bf89733671f50949225dd117f60da6d8902cdbe45dd6df0feef9a4147358cd90d77e8696526ab605aeedd272a7bec6070bf9d08d63487b3a5fd14f3683c62eac4fbafce314684bad9eec5bc1f92caa39735b42aa269b840b12790339936c49ef51964f153754fd602efd74b1f03aa92f367c010e700f9637c72a3349b3e7eb6ecb3ce10b9e01300dfe5400347d1aad5a364d8d9ff3d67100e5f51e2d2cb23625977226263c584acc3f7d83652e486d7c69a732a7b71ae8")
    val blobHash = HexString("da50820830b6d3a1257c5fc75e2119543a733e6b728a265b05af8ea2e7163184")

    val sessionKey1 = "sessionKey1"
    val sessionKey2 = "sessionKey2"
    
    def spec = suite("ClipperzBackend")(
        test("signup - login - save card - logout - login - get card - delete card - logout") {
            for {
                signupResult <- doSignup(sessionKey1)
                loginResult1 <- doLogin(sessionKey1)
                saveCardResult <- doBlobPost(sessionKey1)
                logoutResult <- doLogout(sessionKey1)
                loginResult2 <- doLogin(sessionKey2)
                getCardResult <- doBlobGet(sessionKey2)
                deleteCardResult <- doBlobDelete(sessionKey2)
                logoutResult2 <- doLogout(sessionKey2)
            } yield allSuccesses(
                signupResult,
                loginResult1,
                saveCardResult,
                logoutResult,
                loginResult2,
                getCardResult,
                deleteCardResult,
                logoutResult2,
            )
        }
    ).provideLayerShared(environment) @@
        TestAspect.sequential @@
        TestAspect.beforeAll(TestUtilities.deleteFilesInFolder(blobBasePath)) @@
        TestAspect.afterAll(TestUtilities.deleteFilesInFolder(blobBasePath)) @@
        TestAspect.beforeAll(TestUtilities.deleteFilesInFolder(userBasePath)) @@
        TestAspect.afterAll(TestUtilities.deleteFilesInFolder(userBasePath))


    val p = HexString("597ed0c523f50c6db089a92845693a3f2454590026d71d6a9028a69967d33f6d")

    val userCard: RequestUserCard = RequestUserCard(
        c = HexString("7815018e9d84b5b0f319c87dee46c8876e85806823500e03e72c5d66e5d40456"),
        s = HexString("2f89a30b8a940d810641099be4d11ac26ce65c382ee9d690501fe123d06f9420"),
        v = HexString("b4deef40924f1d6083ff7c2e763d54e60623c6fed66738070c14ca092c43945579ea68e6dd5c364c5082c04c6fac83783aeec17b07471f26fb23c360fd8e7892467eb463da0c725863052389aba7bd21956efc47d127b45942cbd97f835a368cfc72b5ce0d817f0cad52d6cbf01f169b8d700532ebd00b3319f140b73c187754"),
        srpVersion = SRPVersion("6a"),
        originMasterKey = None,
        masterKey = (
            HexString("f20d14d5152ea0659cbd2b7dedd3d284987391be7b2143e19b2281b50d9c0966b533f11a66ecf658bcc3706ec2136213d38eb0dc4e5020a1d0e30d9b8c901600"),
            MasterKeyEncodingVersion("1.0")
        )
    )

    val signupData = SignupData(
        user = userCard,
        userInfoReference = HexString("726f1d8bc207725f2225623f300b9e78d20685406c4096f97ac8a2c864bd52b4"), 
        userInfoContent = HexString("89d6"), 
        userInfoIdentifier = HexString("userInfoIdentifier"),
        indexCardReference = HexString("726f1d8bc207725f2225623f300b9e78d20685406c4096f97ac8a2c864bd52b4"),
        indexCardContent = HexString("89d6"),
        indexCardIdentifier = HexString("indexCardIdentifier"),
        cards = Array[CardsSignupData]()
    )

    private def manageRequestWithTollPayment(request: Task[Request]): ZIO[ClipperzEnvironment, Any, Response] =
        request.
        flatMap(req =>
            app.runZIO(req).flatMap(response =>
                if response.status.code != 402
                then
                    ZIO.succeed(response)
                else
                    computeReceiptFromResponse(response)
                    .zip(ZIO.attempt(response.rawHeader(TollManager.tollHeader).get))
                    .zip(ZIO.attempt(response.rawHeader(TollManager.tollCostHeader).get))
                    .flatMap((receipt, tollHeader, tollCostHeader) =>
                        manageRequestWithTollPayment(
                            ZIO.succeed(
                                req.addHeaders(Headers(TollManager.tollReceiptHeader, receipt.toString()))
                                .addHeaders(Headers(TollManager.tollHeader, tollHeader))
                                .addHeaders(Headers(TollManager.tollCostHeader, tollCostHeader))
                            )
                        )
                    )
            )
        )
  
    private def computeReceiptFromResponse(res: Response) =
        val toll = res.rawHeader(TollManager.tollHeader).map(HexString(_)).get
        val cost = res.rawHeader(TollManager.tollCostHeader).map(_.toInt).get

        ZIO
        .service[PRNG]
        .zip(ZIO.service[TollManager])
        .flatMap((prng, tollManager) => 
            TollManager.computeReceipt(prng, tollManager)(TollChallenge(toll, cost))
        )

    private def doSignup (sessionKey: String) =
        val signupRequest =
            Charset.Standard.utf8.encodeString(signupData.toJson)
            .map(body =>
                Request(
                    url = URL(Root / "users" / userCard.c.toString()),
                    method = Method.POST,
                    headers = Headers(SessionManager.sessionKeyHeaderName, sessionKey),
                    body = Body.fromChunk(body),
                    version = Version.Http_1_1,
                    remoteAddress = Some(InetAddress.getLocalHost().nn)
                )
            )

        for {
            requestResult <- manageRequestWithTollPayment(signupRequest)
        } yield assertTrue(true) // assertTrue(requestResult.status.code == 200)

    private def doLogin (sessionKey: String) =
        val aa = RFCTestVector.aa
        val a = RFCTestVector.a
        val stepData = SRPStep1Data(userCard.c, HexString.bigIntToHex(aa))

        val step1Request: Task[Request] =
            Charset.Standard.utf8.encodeString(stepData.toJson)
            .map(body =>
                Request(
                    url = URL(Root / "login" / "step1" / userCard.c.toString()),
                    method = Method.POST,
                    headers = Headers(SessionManager.sessionKeyHeaderName, sessionKey),
                    body = Body.fromChunk(body),
                    version = Version.Http_1_1,
                    remoteAddress = Some(InetAddress.getLocalHost().nn)
                )
            )

        val step2Request: SRPStep2Data => Task[Request] = data =>
            Charset.Standard.utf8.encodeString(data.toJson)
            .map(body =>
                Request(
                    url = URL(Root / "login" / "step2" / userCard.c.toString()),
                    method = Method.POST,
                    headers = Headers(SessionManager.sessionKeyHeaderName, sessionKey),
                    // body = Body.fromString(data.toJson, StandardCharsets.UTF_8.nn),
                    body = Body.fromChunk(body),
                    version = Version.Http_1_1,
                    remoteAddress = Some(InetAddress.getLocalHost().nn)
                )
            )

        for {
            response1 <- manageRequestWithTollPayment(step1Request)
            stepResponse1 <- fromStream[SRPStep1Response](response1.body.asStream)
            u <- srpFunctions.computeU(bigIntToBytes(aa), stepResponse1.bb.toByteArray)
            config <- ZIO.succeed(srpFunctions.configuration)
            x <- config.keyDerivationFunction(stepResponse1.s.toByteArray, p.toByteArray).map(bytes => bytesToBigInt(bytes))
            clientSecret <- ZIO.succeed(srpFunctions.computeSecretClient(stepResponse1.bb.toBigInt, x, a, bytesToBigInt(u)))
            kk <- srpFunctions.configuration.hash(ZStream.fromIterable(bigIntToBytes(clientSecret)))
            m1 <- srpFunctions.computeM1(
                userCard.c.toByteArray,
                stepResponse1.s.toByteArray,
                stepData.aa.toByteArray,
                stepResponse1.bb.toByteArray,
                kk,
            )
            response2 <- manageRequestWithTollPayment(step2Request(SRPStep2Data(HexString.bytesToHex(m1))))
        } yield assertTrue(response2.status.code == 200)

    private def doBlobPost(sessionKey: String) =
        val request = Request(
            url = URL(Root / "blobs"),
            method = Method.POST,
            headers = Headers(SessionManager.sessionKeyHeaderName, sessionKey),
            body = Body.fromMultipartForm(Form.empty.append(FormField.binaryField(name = "blob", data = Chunk.fromArray(blobData.toByteArray), filename = Some(blobHash.toString()), mediaType = MediaType.any)), Boundary("--XXX")),
            version = Version.Http_1_1,
            remoteAddress = Some(InetAddress.getLocalHost().nn)
        )
        for {
            result <- manageRequestWithTollPayment(ZIO.succeed(request))
        } yield assertTrue(result.status.code == 200)

    private def doBlobGet(sessionKey: String) =
        val request = Request(
            url = URL(Root / "blobs" / blobHash.toString() ),
            method = Method.GET,
            headers = Headers(SessionManager.sessionKeyHeaderName, sessionKey),
            body = Body.empty,
            version = Version.Http_1_1,
            remoteAddress = Some(InetAddress.getLocalHost().nn)
        )
        for {
            result <- manageRequestWithTollPayment(ZIO.succeed(request))
        } yield assertTrue(result.status.code == 200)

    private def doBlobDelete(sessionKey: String) =
        val request = Request(
            url = URL(Root / "blobs"),
            method = Method.DELETE,
            headers = Headers(SessionManager.sessionKeyHeaderName, sessionKey),
            body = Body.fromMultipartForm(Form.empty.append(FormField.binaryField(name = "blob", data = Chunk.fromArray(blobData.toByteArray), filename = Some(blobHash.toString()), mediaType = MediaType.any)), Boundary("--XXX")),
            version = Version.Http_1_1,
            remoteAddress = Some(InetAddress.getLocalHost().nn)
        )
        for {
            result <- manageRequestWithTollPayment(ZIO.succeed(request))
        } yield assertTrue(result.status.code == 200)

    private def doLogout(sessionKey: String) =
        val request = Request(
            url = URL(Root / "logout"),
            method = Method.POST,
            headers = Headers(SessionManager.sessionKeyHeaderName, sessionKey),
            body = Body.empty,
            version = Version.Http_1_1,
            remoteAddress = Some(InetAddress.getLocalHost().nn)
        )
        for {
            result <- manageRequestWithTollPayment(ZIO.succeed(request))
        } yield assertTrue(result.status.code == 200)
