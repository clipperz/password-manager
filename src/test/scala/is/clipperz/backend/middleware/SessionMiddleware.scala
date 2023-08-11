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
import zio.test.TestResult.all
import zio.json.EncoderOps
import zio.http.{ Version, Headers, Method, URL, Request, Body }
import zio.http.*
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
}
import is.clipperz.backend.services.TollChallenge
import is.clipperz.backend.services.TollReceipt
import is.clipperz.backend.services.Session
import java.net.Inet4Address
import java.net.InetAddress

object SessionMiddlewareSpec extends ZIOSpecDefault:
  val layers = SessionManager.live

  val sessionKey = "sessionKey"

  def getFromPath(path: String): Request =
    Request(
      url = URL(!! / path / "4073041693a9a66983e6ffb75b521310d30e6db60afc0f97d440cb816bce7c63"),
      method = Method.GET,
      headers = Headers.empty,
      body = Body.empty,
      version = Version.Http_1_1,
      remoteAddress = Some(InetAddress.getLocalHost().nn)
    )

  def withSessionFromPath(path: String, method: Method): Request =
    Request(
      url = URL(!! / path / "4073041693a9a66983e6ffb75b521310d30e6db60afc0f97d440cb816bce7c63"),
      method = method,
      headers = Headers((SessionManager.sessionKeyHeaderName, sessionKey)),
      body = Body.empty,
      version = Version.Http_1_1,
      remoteAddress = Some(InetAddress.getLocalHost().nn)
    )

  val getWithSession = withSessionFromPath("blobs", Method.GET)

  val get = getFromPath("ciao")

  val createSession = Request(
    url = URL(!! / "create" / sessionKey),
    method = Method.GET,
    headers = Headers((SessionManager.sessionKeyHeaderName, sessionKey)),
    body = Body.empty,
    version = Version.Http_1_1,
    remoteAddress = Some(InetAddress.getLocalHost().nn)
  )

  val createSessionApi: HttpApp[SessionManager, Throwable] = Http.collectZIO {
    case request @ Method.GET -> !! / "create" / key =>
      ZIO
        .service[SessionManager]
        .flatMap(sessionManager => sessionManager.getSession(request).map((sessionManager, _)))
        .flatMap((sessionManager, session) => sessionManager.saveSession(Session(session._1, Map(("c", key)))))
        .map(_ => Response.ok)
  }

  val idApp: HttpApp[SessionManager, Throwable] = createSessionApi ++ (Http.fromHandler(Handler.ok) @@ sessionChecks)

  val sessionHeaderNecessary = List("users", "login", "blobs", "logout")
  val validSessionNecessary = List(
    ("blobs", List(Method.PUT, Method.POST, Method.GET, Method.DELETE)),
    ("logout", List(Method.POST)),
    ("users", List(Method.PUT, Method.GET, Method.DELETE)),
  )
  val validSessionNonNecessary = List(("login", List(Method.POST)), ("users", List(Method.POST)))

  def spec = suite("SessionMiddleware")(
    test("200 if no session header when not necessary") {
      assertZIO(idApp.runZIO(get).map(res => res.status == Status.Ok))(isTrue)
    },
    test("400 if no session header when necessary") {
      sessionHeaderNecessary
        .map(path => assertZIO(idApp.runZIO(getFromPath(path)).map(res => res.status == Status.BadRequest))(isTrue))
        .reduce((zio1, zio2) => zio1.flatMap(z1 => zio2.map(z1 && _)))
    },
    test("401 if session is not present/empty when it should be valid") {
      validSessionNecessary
        .map((path, methods) =>
          methods
            .map(method =>
              assertZIO(idApp.runZIO(withSessionFromPath(path, method)).map(res => res.status == Status.Unauthorized))(isTrue)
            )
            .reduce((zio1, zio2) => zio1.flatMap(z1 => zio2.map(z1 && _)))
        )
        .reduce((zio1, zio2) => zio1.flatMap(z1 => zio2.map(z1 && _)))
    },
    test("200 if session is not present/empty when it is not necessary for it to be valid") {
      validSessionNonNecessary
        .map((path, methods) =>
          methods
            .map(method => assertZIO(idApp.runZIO(withSessionFromPath(path, method)).map(res => res.status == Status.Ok))(isTrue))
            .reduce((zio1, zio2) => zio1.flatMap(z1 => zio2.map(z1 && _)))
        )
        .reduce((zio1, zio2) => zio1.flatMap(z1 => zio2.map(z1 && _)))
    },
    test("200 if session is present and correct") {
      assertZIO(idApp.runZIO(createSession).flatMap(_ => idApp.runZIO(getWithSession).map(res => res.status == Status.Ok)))(isTrue)
    },
  ).provideSomeLayer(layers) @@
    TestAspect.sequential
