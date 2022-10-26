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
import zhttp.http.{ Version, Headers, Method, URL, Request, HttpData }
import zhttp.http.*
import is.clipperz.backend.Main
import is.clipperz.backend.data.HexString
import is.clipperz.backend.data.HexString.{ bytesToHex }
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
import is.clipperz.backend.middleware.{ hashcash, tollPresentMiddleware, correctReceiptMiddleware, missingTollMiddleware, wrongTollMiddleware, checkReceipt }
import is.clipperz.backend.services.TollChallenge
import is.clipperz.backend.services.TollReceipt
import is.clipperz.backend.services.Session

object SessionMiddlewareSpec extends ZIOSpecDefault:
  val layers = SessionManager.live

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

  val createSession = Request(
    url = URL(!! / "create" / sessionKey),
    method = Method.GET,
    headers = Headers((SessionManager.sessionKeyHeaderName, sessionKey)),
    version = Version.Http_1_1,
  )

  val createSessionApi: HttpApp[SessionManager, Throwable] = Http.collectZIO {
    case request @ Method.GET -> !! / "create" / key =>
      ZIO
        .service[SessionManager]
        .flatMap(sessionManager => sessionManager.getSession(key).map((sessionManager, _)))
        .flatMap((sessionManager, session) => 
          sessionManager.saveSession(Session(session._1, Map(("c", key))))
        )
        .map(_ => Response.ok)
  }

  val idApp: HttpApp[SessionManager, Throwable] = createSessionApi ++ (Http.ok @@ sessionChecks)

  def spec = suite("SessionMiddleware")(
    test("400 if no session header") {
      assertZIO(idApp(get).map(res => res.status == Status.BadRequest))(isTrue)
    },
    test("401 if session is not present/empty") {
      assertZIO(idApp(getWithSession).map(res => res.status == Status.Unauthorized))(isTrue)
    },
    test("200 if session is present and correct") {
      assertZIO(idApp(createSession).flatMap(_ => idApp(getWithSession).map(res => res.status == Status.Ok)))(isTrue)
    }
  ).provideSomeLayer(layers) @@ 
    TestAspect.sequential

