package is.clipperz.backend.services

import java.io.File
// import java.nio.charset.StandardCharsets
// import java.nio.file.{ Files, Paths, FileSystems }
import java.security.MessageDigest
import scala.language.postfixOps
import zio.{ Chunk, ZIO }
import zio.stream.{ ZStream, ZSink }
import zio.test.TestResult.{ allSuccesses }
import zio.test.Assertion.{ nothing, throws, throwsA, fails, isSubtype, anything }
import zio.test.{ ZIOSpecDefault, assertTrue, assert, assertCompletes, assertNever, assertZIO, check, TestAspect, Gen }
import zio.json.EncoderOps
import zio.http.{ Version, Headers, Method, URL, Request, Body }
import zio.http.*
import is.clipperz.backend.Main
// import java.nio.file.Path
import _root_.is.clipperz.backend.Exceptions.*
import zio.Clock
import zio.Clock.ClockLive
import zio.test.TestClock
import zio.Duration
import zio.durationInt
import scala.collection.immutable.HashMap

object SessionManagerSpec extends ZIOSpecDefault:
  val testSessionKey = "testSessionKey"
  val c = "username"
  val cfail = "username2"
  val sessionContent = Map(("c", c))
  val testSession = Session(testSessionKey, sessionContent)
  val emptyTestSession = Session(testSessionKey, HashMap.empty)
  val cacheTimeToLive = 5.seconds

  def testRequest(key: String) = Request(
    url = URL(Root),
    method = Method.GET,
    headers = Headers((SessionManager.sessionKeyHeaderName, key)),
    body = Body.empty,
    version = Version.Http_1_1,
    remoteAddress = None
  )

  val layers = (PRNG.live >>> SessionManager.live(cacheTimeToLive))

  def spec = (suite("SessionManager")(
    test("get session - empty") {
        for {
            manager <- ZIO.service[SessionManager]
            session <- manager.getSession(testRequest(testSessionKey))
        } yield assertTrue(session.isEmpty)
    },
    test("save/get session - success") {
        for {
            manager      <- ZIO.service[SessionManager]
            savedKey     <- manager.saveSession(testSession)
            savedSession <- manager.getSession(testRequest(savedKey))
        } yield assertTrue(savedSession == testSession, savedKey == testSessionKey)
    },
    test("verifySessionUser - success") {
        for {
            manager <- ZIO.service[SessionManager]
        } yield allSuccesses(
            assertCompletes
        ,   assertTrue(manager.verifySessionUser(c, testSession))
        )
    },
    test("verifySessionUser - fail - different C") {
        for {
            manager <- ZIO.service[SessionManager]
        } yield assertTrue(manager.verifySessionUser(cfail, testSession) == false)
    },
    test("deleteSession - success") {
        for {
            manager      <- ZIO.service[SessionManager]
            key          <- manager.saveSession(testSession)
            _            <- manager.deleteSession(testRequest(key))
            savedSession <- manager.getSession(testRequest(key))
        } yield assertTrue(savedSession.isEmpty)
    },
    test("verifySessionUser - fail - no c") {
        for {
            manager <- ZIO.service[SessionManager]
        } yield assertTrue(manager.verifySessionUser(c, emptyTestSession) == false)
    },
    test("session is deleted after timeToLive") {
        for {
            manager      <- ZIO.service[SessionManager]
            savedKey     <- manager.saveSession(testSession)
            _            <- TestClock.adjust(cacheTimeToLive.plusNanos(1).nn)
            savedSession <- manager.getSession(testRequest(savedKey))
        } yield assertTrue(savedSession.isEmpty)
    },
    test("session timeout is refreshed when used") {
        for {
            manager      <- ZIO.service[SessionManager]
            savedKey     <- manager.saveSession(testSession)
            _            <- TestClock.adjust(cacheTimeToLive.minusSeconds(1).nn)
            _            <- manager.getSession(testRequest(savedKey))
            _            <- TestClock.adjust(cacheTimeToLive)
            savedSession <- manager.getSession(testRequest(savedKey))
        } yield assertTrue(savedSession == testSession)
    }
  )).provideLayer(layers)
