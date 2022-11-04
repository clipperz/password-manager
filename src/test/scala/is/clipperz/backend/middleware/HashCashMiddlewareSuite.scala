package is.clipperz.backend.middleware

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Paths, FileSystems }
import java.security.MessageDigest
import scala.language.postfixOps
import zio.{ Chunk, ZIO, Task }
import zio.stream.{ ZStream, ZSink }
import zio.test.Assertion.{ nothing, isTrue }
import zio.test.{ ZIOSpecDefault, assertTrue, assertNever, assert, assertZIO, TestAspect }
import zio.json.EncoderOps
import zhttp.http.{ Version, Headers, Method, URL, Request, Body }
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
import is.clipperz.backend.services.tollByteSize
import is.clipperz.backend.services.SrpManager
import is.clipperz.backend.middleware.{
  hashcash,
  tollPresentMiddleware,
  correctReceiptMiddleware,
  missingTollMiddleware,
  wrongTollMiddleware,
  checkReceipt,
}
import is.clipperz.backend.services.TollChallenge
import is.clipperz.backend.services.TollReceipt

object HashCashMiddlewareSpec extends ZIOSpecDefault:
  val layers =
    PRNG.live ++
      SessionManager.live ++
      (PRNG.live >>> TollManager.live)

  val sessionKey = "sessionKey"

  val get = Request(
    url = URL(!! / "blobs" / "4073041693a9a66983e6ffb75b521310d30e6db60afc0f97d440cb816bce7c63"),
    method = Method.GET,
    headers = Headers.empty,
    version = Version.Http_1_1,
  )

  val getWithSession = Request(
    url = URL(!! / "blobs" / "4073041693a9a66983e6ffb75b521310d30e6db60afc0f97d440cb816bce7c63"),
    method = Method.GET,
    headers = Headers((SessionManager.sessionKeyHeaderName, sessionKey)),
    version = Version.Http_1_1,
  )

  val getWithFixedToll = Request(
    url = URL(!! / "blobs" / "4073041693a9a66983e6ffb75b521310d30e6db60afc0f97d440cb816bce7c63"),
    method = Method.GET,
    headers = Headers((SessionManager.sessionKeyHeaderName, sessionKey), (TollManager.tollReceiptHeader, "12345678901234567890")),
    version = Version.Http_1_1,
  )

  val idApp: HttpApp[TollManager & SessionManager, Throwable] = Http.ok @@ hashcash

  def spec = suite("HashCashMiddleware")(
    test("400 if no session is active") {
      assertZIO(idApp(get).map(res => res.status == Status.BadRequest))(isTrue)
    },
    test("402 if hashcash is missing") {
      assertZIO(idApp(getWithSession).map(res => res.status == Status.PaymentRequired))(isTrue)
    },
    test("New challenge received if hashcash is missing") {
      val response = idApp(getWithSession)
      assertZIO(response.map(res => res.headers.hasHeader(TollManager.tollHeader)))(isTrue)
      assertZIO(response.map(res => res.headers.hasHeader(TollManager.tollCostHeader)))(isTrue)
    },
    test("400 if hashcash present but never issued") {
      assertZIO(idApp(getWithFixedToll).map(res => res.status == Status.BadRequest))(isTrue)
    },
    test("402 if hashcash is present but incorrect") {
      for {
        prng <- ZIO.service[PRNG]
        tollManager <- ZIO.service[TollManager]
        response <- idApp(getWithSession)
        toll <- ZIO.attempt(response.headerValue(TollManager.tollHeader).map(HexString(_)).get)
        cost <- ZIO.attempt(response.headerValue(TollManager.tollCostHeader).map(_.toInt).get)
        receipt <- computeWrongReceipt(prng, tollManager)(TollChallenge(toll, cost))
        newGet <- ZIO.succeed(
          Request(
            url = URL(!! / "blobs" / "4073041693a9a66983e6ffb75b521310d30e6db60afc0f97d440cb816bce7c63"),
            method = Method.GET,
            headers = Headers(
              (SessionManager.sessionKeyHeaderName, getWithSession.headerValue(SessionManager.sessionKeyHeaderName).get),
              (TollManager.tollHeader, toll.toString()),
              (TollManager.tollCostHeader, cost.toString()),
              (TollManager.tollReceiptHeader, receipt.toString()),
            ),
            version = Version.Http_1_1,
          )
        )
        response2 <- idApp(newGet)
      } yield assertTrue(response2.status == Status.PaymentRequired)
    },
    test("400 if hashcash challenge is not the one issued") {
      for {
        prng <- ZIO.service[PRNG]
        tollManager <- ZIO.service[TollManager]
        response <- idApp(getWithSession)
        toll <- ZIO.attempt(response.headerValue(TollManager.tollHeader).map(HexString(_)).get)
        cost <- ZIO.attempt(response.headerValue(TollManager.tollCostHeader).map(_.toInt).get)
        receipt <- computeWrongReceipt(prng, tollManager)(TollChallenge(toll, cost))
        newGet <- ZIO.succeed(
          Request(
            url = URL(!! / "blobs" / "4073041693a9a66983e6ffb75b521310d30e6db60afc0f97d440cb816bce7c63"),
            method = Method.GET,
            headers = Headers(
              (SessionManager.sessionKeyHeaderName, getWithSession.headerValue(SessionManager.sessionKeyHeaderName).get),
              (TollManager.tollHeader, s"cc${toll.toString()}"),
              (TollManager.tollCostHeader, s"ss${cost.toString()}"),
              (TollManager.tollReceiptHeader, receipt.toString()),
            ),
            version = Version.Http_1_1,
          )
        )
        response2 <- idApp(newGet)
      } yield assertTrue(response2.status == Status.BadRequest)
    },
    test("200 if hashcash is present and correct") {
      for {
        prng <- ZIO.service[PRNG]
        tollManager <- ZIO.service[TollManager]
        response <- idApp(getWithSession)
        toll <- ZIO.attempt(response.headerValue(TollManager.tollHeader).map(HexString(_)).get)
        cost <- ZIO.attempt(response.headerValue(TollManager.tollCostHeader).map(_.toInt).get)
        receipt <- TollManager.computeReceipt(prng, tollManager)(TollChallenge(toll, cost))
        newGet <- ZIO.succeed(
          Request(
            url = URL(!! / "blobs" / "4073041693a9a66983e6ffb75b521310d30e6db60afc0f97d440cb816bce7c63"),
            method = Method.GET,
            headers = Headers(
              (SessionManager.sessionKeyHeaderName, getWithSession.headerValue(SessionManager.sessionKeyHeaderName).get),
              (TollManager.tollHeader, toll.toString()),
              (TollManager.tollCostHeader, cost.toString()),
              (TollManager.tollReceiptHeader, receipt.toString()),
            ),
            version = Version.Http_1_1,
          )
        )
        response2 <- idApp(newGet)
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