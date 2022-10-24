package is.clipperz.backend.services

import org.scalacheck.Test

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Paths, FileSystems }
import java.security.MessageDigest
import scala.language.postfixOps
import zio.{ Chunk, ZIO }
import zio.stream.{ ZStream, ZSink }
import zio.test.Assertion.{ nothing, throws, throwsA, fails, isSubtype, anything }
import zio.test.{ ZIOSpecDefault, assertTrue, assert, assertCompletes, assertZIO, TestAspect }
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
import is.clipperz.backend.data.HexString
import is.clipperz.backend.exceptions.BadRequestException
import zio.test.TestEnvironment
import zio.ZLayer

object UserArchiveSpec extends ZIOSpecDefault:
  val userBasePath = FileSystems.getDefault().nn.getPath("target", "tests", "archive", "users").nn

  val environment = UserArchive.fs(userBasePath, 2)

  val c = HexString("abcdef0192837465")
  val testUser = UserCard(c, HexString("s"), HexString("v"), "srpVersion_test", "masterKeyEncodingVersion_test", HexString("masterKeyContent_test"))
  val testUser2 = UserCard(c, HexString("s2"), HexString("v"), "srpVersion_test", "masterKeyEncodingVersion_test", HexString("masterKeyContent_test"))

 
  def spec = suite("UserArchive")(
    test("getUser - fail") {
      for {
        archive <- ZIO.service[UserArchive]
        user <- archive.getUser(c)
      } yield assertTrue(user == None)
    } +
    test("saveBlob - success") {
      for {
        archive <- ZIO.service[UserArchive]
        fiber <- archive.saveUser(testUser, false).fork
        _ <- TestClock.adjust(Duration.fromMillis(10))
        _ <- fiber.join
        user <- archive.getUser(c)
      } yield assertTrue(user == Some(testUser))
    } + 
    test ("saveBlob with failing stream - success") {
      for {
       archive <- ZIO.service[UserArchive]
       fiber <- archive.saveBlob(failingKey, failingContent).fork
        _ <- TestClock.adjust(Duration.fromMillis(10))
        res <- assertZIO(fiber.await)(fails(isSubtype[EmptyContentException](anything)))
      } yield assertTrue(res.isSuccess)
    } + 
    test ("saveBlob with wrong hash - success") {
      for {
       archive <- ZIO.service[UserArchive]
       fiber <- archive.saveBlob(failingKey, testContent).fork
        _ <- TestClock.adjust(Duration.fromMillis(10))
        res <- assertZIO(fiber.await)(fails(isSubtype[BadRequestException](anything)))
      } yield assertTrue(res.isSuccess)
    } +
    test("getUser - success") {
      for {
        archive <- ZIO.service[UserArchive]
        user <- archive.getUser(c)
      } yield assertTrue(user == Some(testUser))
    } + 
    test("deleteBlob - success") {
      for {
        archive <- ZIO.service[UserArchive]
        res <- archive.deleteBlob(testContent)
      } yield assertTrue(res)
    } + 
    test("deleteBlob - fail") {
      for {
        archive <- ZIO.service[UserArchive]
        res <- archive.deleteBlob(testContent)
      } yield assertTrue(!res)
    }
  ).provideSomeLayerShared(environment) @@ 
    TestAspect.sequential @@ 
    TestAspect.beforeAll(ZIO.succeed(FileSystem.deleteAllFiles(userBasePath.toFile().nn))) @@
    TestAspect.afterAll(ZIO.succeed(FileSystem.deleteAllFiles(userBasePath.toFile().nn)))
