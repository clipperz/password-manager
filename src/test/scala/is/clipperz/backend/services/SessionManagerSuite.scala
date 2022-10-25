package is.clipperz.backend.services

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Paths, FileSystems }
import java.security.MessageDigest
import scala.language.postfixOps
import zio.{ Chunk, ZIO }
import zio.stream.{ ZStream, ZSink }
import zio.test.Assertion.{ nothing, throws, throwsA, fails, isSubtype, anything }
import zio.test.{ ZIOSpecDefault, assertTrue, assert, assertCompletes, assertNever, assertZIO, TestAspect }
import zio.json.EncoderOps
import zhttp.http.{ Version, Headers, Method, URL, Request, HttpData }
import zhttp.http.*
import is.clipperz.backend.Main
import java.nio.file.Path
import _root_.is.clipperz.backend.exceptions.ResourceNotFoundException
import is.clipperz.backend.functions.FileSystem
import is.clipperz.backend.exceptions.EmptyContentException
import zio.Clock
import zio.Clock.ClockLive
import zio.test.TestClock
import zio.Duration
import is.clipperz.backend.exceptions.BadRequestException

object SessionManagerSpec extends ZIOSpecDefault:
  val sessionKey = "sessionKey"
  val c = "username"
  val cfail = "username2"
  val sessionContent = Map(("c", c))
  val testSession = Session(sessionKey, sessionContent)
  val testRequestSuccess = Request(
      url = URL(!! / "users" / c),
      method = Method.GET,
      headers = Headers((SessionManager.sessionKeyHeaderName, sessionKey)),
      version = Version.Http_1_1,
    )
  val testRequestFail = Request(
      url = URL(!! / "users" / cfail),
      method = Method.GET,
      headers = Headers((SessionManager.sessionKeyHeaderName, sessionKey)),
      version = Version.Http_1_1,
    )
  val testRequestNoHeader = Request(
      url = URL(!! / "users" / (c + "fail")),
      method = Method.GET,
      headers = Headers.empty,
      version = Version.Http_1_1,
    )

  val layers = SessionManager.live

  def spec = suite("SessionManager")(
    test("getSession - empty") {
      for {
        manager <- ZIO.service[SessionManager]
        session <- manager.getSession(sessionKey)
      } yield assertTrue(session.content.isEmpty)
    } + 
    test("saveSession - success") {
      for {
        manager <- ZIO.service[SessionManager]
        savedKey <- manager.saveSession(testSession)
        savedSession <- manager.getSession(testSession.key)
      } yield assertTrue(savedKey == testSession.key, savedSession == testSession)
    } +
    test("getSession - success") {
      for {
        manager <- ZIO.service[SessionManager]
        session <- manager.getSession(sessionKey)
      } yield assertTrue(session == testSession)
    } +
    test("verifySessionUser - success") {
      for {
        manager <- ZIO.service[SessionManager]
        _ <- manager.verifySessionUser(c, testRequestSuccess)
      } yield assertCompletes
    } +
    test("verifySessionUser - fail - different C") {
      for {
        manager <- ZIO.service[SessionManager]
        res <- assertZIO(manager.verifySessionUser(cfail, testRequestFail).exit)(fails(isSubtype[BadRequestException](anything)))
      } yield assertTrue(res.isSuccess)
    } +
    test("deleteSession - success") {
      for {
        manager <- ZIO.service[SessionManager]
        _ <- manager.deleteSession(sessionKey)
        savedSession <- manager.getSession(sessionKey)
      } yield assertTrue(savedSession.content.isEmpty)
    } +
    test("verifySessionUser - fail - no c") {
      for {
        manager <- ZIO.service[SessionManager]
        res <- assertZIO(manager.verifySessionUser(c, testRequestSuccess).exit)(fails(isSubtype[BadRequestException](anything)))
      } yield assertTrue(res.isSuccess)
    } +
    test("verifySessionUser - fail - no header") {
      for {
        manager <- ZIO.service[SessionManager]
        res <- assertZIO(manager.verifySessionUser(c, testRequestNoHeader).exit)(fails(isSubtype[BadRequestException](anything)))
      } yield assertTrue(res.isSuccess)
    }
  ).provideLayerShared(layers) @@ TestAspect.sequential 
