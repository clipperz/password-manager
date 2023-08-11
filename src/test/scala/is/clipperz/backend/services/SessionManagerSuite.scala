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
import zio.http.{ Version, Headers, Method, URL, Request, Body }
import zio.http.*
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

  val testRequestEmpty = Request(
    url = URL(!!),
    method = Method.GET,
    headers = Headers((SessionManager.sessionKeyHeaderName, sessionKey)),
    body = Body.empty,
    version = Version.Http_1_1,
    remoteAddress = None
  )
  val testRequestSuccess = Request(
    url = URL(!! / "users" / c),
    method = Method.GET,
    headers = Headers((SessionManager.sessionKeyHeaderName, sessionKey)),
    body = Body.empty,
    version = Version.Http_1_1,
    remoteAddress = None
  )
  val testRequestFail = Request(
    url = URL(!! / "users" / cfail),
    method = Method.GET,
    headers = Headers((SessionManager.sessionKeyHeaderName, sessionKey)),
    body = Body.empty,
    version = Version.Http_1_1,
    remoteAddress = None
  )
  val testRequestNoHeader = Request(
    url = URL(!! / "users" / (c + "fail")),
    method = Method.GET,
    headers = Headers.empty,
    body = Body.empty,
    version = Version.Http_1_1,
    remoteAddress = None
  )

  val layers = SessionManager.live

  def spec = suite("SessionManager")(
    test("getSession - empty") {
      for {
        manager <- ZIO.service[SessionManager]
        session <- manager.getSession(testRequestEmpty)
      } yield assertTrue(session.content.isEmpty)
    } +
    test("saveSession - success") {
      for {
        manager <- ZIO.service[SessionManager]
        savedKey <- manager.saveSession(testSession)
        savedSession <- manager.getSession(testRequestEmpty)
      } yield assertTrue(savedKey == testSession.key, savedSession == testSession)
    } +
    test("getSession - success") {
      for {
        manager <- ZIO.service[SessionManager]
        session <- manager.getSession(testRequestEmpty)
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
        res <- assertZIO(manager.verifySessionUser(cfail, testRequestFail).exit)(
          fails(isSubtype[BadRequestException](anything))
        )
      } yield res
    } +
    test("deleteSession - success") {
      for {
        manager <- ZIO.service[SessionManager]
        _ <- manager.deleteSession(testRequestEmpty)
        savedSession <- manager.getSession(testRequestEmpty)
      } yield assertTrue(savedSession.content.isEmpty)
    } +
    test("verifySessionUser - fail - no c") {
      for {
        manager <- ZIO.service[SessionManager]
        res <- assertZIO(manager.verifySessionUser(c, testRequestSuccess).exit)(fails(isSubtype[BadRequestException](anything)))
      } yield res
    } +
    test("verifySessionUser - fail - no header") {
      for {
        manager <- ZIO.service[SessionManager]
        res <- assertZIO(manager.verifySessionUser(c, testRequestNoHeader).exit)(
          fails(isSubtype[BadRequestException](anything))
        )
      } yield res
    }
  ).provideLayerShared(layers) @@ TestAspect.sequential
