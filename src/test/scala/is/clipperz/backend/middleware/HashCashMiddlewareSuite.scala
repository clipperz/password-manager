package is.clipperz.backend.middleware

import java.io.File
// import java.nio.charset.StandardCharsets
// import java.nio.file.{ Files, Paths, FileSystems }
import java.security.MessageDigest
import scala.language.postfixOps
import zio.{ Chunk, ZIO, Task }
import zio.stream.{ ZStream, ZSink }
import zio.test.Assertion.{ nothing, isTrue }
import zio.test.{ ZIOSpecDefault, assertTrue, assertNever, assert, assertZIO, TestAspect }
import zio.json.EncoderOps
import zio.http.{ Version, Headers, Handler, Method, URL, Request, Body }
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
import is.clipperz.backend.services.tollByteSize
import is.clipperz.backend.services.SrpManager
// import is.clipperz.backend.middleware.{ hashcash }
import is.clipperz.backend.services.TollChallenge
import is.clipperz.backend.services.TollReceipt
import java.net.InetAddress
import is.clipperz.backend.services.ChallengeType

object HashCashMiddlewareSpec extends ZIOSpecDefault:
  val layers =
    PRNG.live ++
      (PRNG.live >>> SessionManager.live()) ++
      (PRNG.live >>> TollManager.live)

  val sessionKey = "____sessionKey____"

  val get = Request(
    url = URL(Root / "api" / "blobs" / "4073041693a9a66983e6ffb75b521310d30e6db60afc0f97d440cb816bce7c63"),
    method = Method.GET,
    headers = Headers.empty,
    body = Body.empty,
    version = Version.Http_1_1,
    remoteAddress = Some(InetAddress.getLocalHost().nn)
  )

  val getWithSession = Request(
    url = URL(Root / "api" / "blobs" / "4073041693a9a66983e6ffb75b521310d30e6db60afc0f97d440cb816bce7c63"),
    method = Method.GET,
    headers = Headers((SessionManager.sessionKeyHeaderName, sessionKey)),
    body = Body.empty,
    version = Version.Http_1_1,
    remoteAddress = Some(InetAddress.getLocalHost().nn)
  )

  val getWithFixedToll = Request(
    url = URL(Root / "api" / "blobs" / "4073041693a9a66983e6ffb75b521310d30e6db60afc0f97d440cb816bce7c63"),
    method = Method.GET,
    headers = Headers((SessionManager.sessionKeyHeaderName, sessionKey)).addHeaders(Headers((TollManager.tollReceiptHeader, "12345678901234567890"))),
    body = Body.empty,
    version = Version.Http_1_1,
    remoteAddress = Some(InetAddress.getLocalHost().nn)
  )

  val idApp: HttpApp[TollManager & SessionManager] = HttpApp.collectZIO(_ => ZIO.succeed(Response.ok)) @@ hashcash(ChallengeType.MESSAGE, ChallengeType.MESSAGE)

  def spec = suite("HashCashMiddleware")(
    test("400 if no session is active") {
        for {
            getStatus <- idApp.runZIO(get).map(res => res.status)
        } yield assertTrue(getStatus == Status.PaymentRequired)
    },
    test("402 if hashcash is missing") {
        for {
            getStatus <- idApp.runZIO(getWithSession).map(res => res.status)
        } yield assertTrue(getStatus == Status.PaymentRequired)
    },
    test("New challenge received if hashcash is missing") {
      val response = idApp.runZIO(getWithSession)
      assertZIO(response.map(res => res.headers.hasHeader(TollManager.tollHeader)))(isTrue)
      assertZIO(response.map(res => res.headers.hasHeader(TollManager.tollCostHeader)))(isTrue)
    },
    test("400 if hashcash present but never issued") {
      assertZIO(idApp.runZIO(getWithFixedToll).map(res => res.status == Status.PaymentRequired))(isTrue)
    },
    test("402 if hashcash is present but incorrect") {
      for {
        prng <- ZIO.service[PRNG]
        tollManager <- ZIO.service[TollManager]
        response <- idApp.runZIO(getWithSession)
        toll <- ZIO.attempt(response.rawHeader(TollManager.tollHeader).map(HexString(_)).get)
        cost <- ZIO.attempt(response.rawHeader(TollManager.tollCostHeader).map(_.toInt).get)
        receipt <- computeWrongReceipt(prng, tollManager)(TollChallenge(toll, cost))
        newGet <- ZIO.succeed(
          Request(
            url = URL(Root / "api" / "blobs" / "4073041693a9a66983e6ffb75b521310d30e6db60afc0f97d440cb816bce7c63"),
            method = Method.GET,
            headers = Headers.empty,
            body = Body.empty,
            version = Version.Http_1_1,
            remoteAddress = None
          )
          .addHeader(SessionManager.sessionKeyHeaderName, getWithSession.rawHeader(SessionManager.sessionKeyHeaderName).get)
          .addHeader(TollManager.tollHeader, toll.toString())
          .addHeader(TollManager.tollCostHeader, cost.toString())
          .addHeader(TollManager.tollReceiptHeader, receipt.toString())
        )
        response2 <- idApp.runZIO(newGet)
      } yield assertTrue(response2.status == Status.PaymentRequired)
    },
    test("400 if hashcash challenge is not the one issued") {
      for {
        prng <- ZIO.service[PRNG]
        tollManager <- ZIO.service[TollManager]
        response <- idApp.runZIO(getWithSession)
        toll <- ZIO.attempt(response.rawHeader(TollManager.tollHeader).map(HexString(_)).get)
        cost <- ZIO.attempt(response.rawHeader(TollManager.tollCostHeader).map(_.toInt).get)
        receipt <- computeWrongReceipt(prng, tollManager)(TollChallenge(toll, cost))
        newGet <- ZIO.succeed(
          Request(
            url = URL(Root / "api" / "blobs" / "4073041693a9a66983e6ffb75b521310d30e6db60afc0f97d440cb816bce7c63"),
            method = Method.GET,
            headers = Headers.empty,
            body = Body.empty,
            version = Version.Http_1_1,
            remoteAddress = None
          )
          .addHeader(SessionManager.sessionKeyHeaderName, getWithSession.rawHeader(SessionManager.sessionKeyHeaderName).get)
          .addHeader(TollManager.tollHeader, s"cc${toll.toString()}")
          .addHeader(TollManager.tollCostHeader, s"ss${cost.toString()}")
          .addHeader(TollManager.tollReceiptHeader, receipt.toString())
        )
        response2 <- idApp.runZIO(newGet)
      } yield assertTrue(response2.status == Status.PaymentRequired)
    },
    test("200 if hashcash is present and correct") {
      for {
        prng <- ZIO.service[PRNG]
        tollManager <- ZIO.service[TollManager]
        response <- idApp.runZIO(getWithSession)
        toll <- ZIO.attempt(response.rawHeader(TollManager.tollHeader).map(HexString(_)).get)
        cost <- ZIO.attempt(response.rawHeader(TollManager.tollCostHeader).map(_.toInt).get)
        receipt <- TollManager.computeReceipt(prng, tollManager)(TollChallenge(toll, cost))
        newGet <- ZIO.succeed(
          Request(
            url = URL(Root / "blobs" / "4073041693a9a66983e6ffb75b521310d30e6db60afc0f97d440cb816bce7c63"),
            method = Method.GET,
            headers = Headers.empty,
            body = Body.empty,
            remoteAddress = None,
            version = Version.Http_1_1,
          )
          .addHeader(SessionManager.sessionKeyHeaderName, getWithSession.rawHeader(SessionManager.sessionKeyHeaderName).get)
          .addHeader(TollManager.tollHeader, toll.toString())
          .addHeader(TollManager.tollCostHeader, cost.toString())
          .addHeader(TollManager.tollReceiptHeader, receipt.toString())
        )
        response2 <- idApp.runZIO(newGet)
      } yield assertTrue(response2.status == Status.Ok)
    },
    // test("402 if repeated hashcash") {
    //   assertNever("Yet to be implemented")
    // },
    // test("Can manage multiple challenges simultaneously") {
    //   assertNever("Yet to be implemented")
    // },
  ).provideSomeLayer(layers) @@
    TestAspect.sequential

  def computeWrongReceipt(prng: PRNG, tollManager: TollManager)(challenge: TollChallenge): Task[TollReceipt] =
    prng
      .nextBytes(tollByteSize)
      .map(HexString.bytesToHex(_))
      .flatMap(receipt =>
        ZIO
          .ifZIO(tollManager.verifyToll(challenge, receipt))
          .apply(
            computeWrongReceipt(prng, tollManager)(challenge),
            ZIO.succeed(receipt),
          )
      )
