package is.clipperz.backend.services

import java.io.File
// import java.nio.charset.StandardCharsets
// import java.nio.file.{ Files, Paths, FileSystems }
import java.security.MessageDigest
import scala.language.postfixOps
import zio.{ Chunk, ZIO }
import zio.stream.{ ZStream, ZSink }
import zio.test.Assertion.{ nothing, throws, throwsA, fails, isSubtype, anything }
import zio.test.{ ZIOSpecDefault, assertTrue, assert, assertCompletes, assertZIO, TestAspect }
import zio.json.EncoderOps
import zio.nio.file.{ Files, FileSystem }
import is.clipperz.backend.Main
// import java.nio.file.Path
import _root_.is.clipperz.backend.Exceptions.*
import zio.Clock
import zio.Clock.ClockLive
import zio.test.TestClock
import zio.Duration
import is.clipperz.backend.data.HexString
import zio.test.TestEnvironment
import zio.ZLayer
import zio.test.TestConsole
import is.clipperz.backend.TestUtilities

object UserArchiveSpec extends ZIOSpecDefault:
  val userBasePath = FileSystem.default.getPath("target", "tests", "archive", "users")

  val keyBlobArchiveFolderDepth = 16
  val environment = UserArchive.fs(userBasePath, keyBlobArchiveFolderDepth, false)

  val c = HexString("abcdef0192837465")
  val testUser = RemoteUserCard(
    c,
    HexString("adc"),
    HexString("adcf"),
    SRPVersion("srpVersion_test"),
    (HexString("masterKeyContent_test"), MasterKeyEncodingVersion("masterKeyEncodingVersion_test"))
  )
  val testUser2 = RemoteUserCard(
    c,
    HexString("adc2"),
    HexString("adcf2"),
    SRPVersion("srpVersion_test"),
    (HexString("masterKeyContent_test"), MasterKeyEncodingVersion("masterKeyEncodingVersion_test"))
  )

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
          res <- fiber.join
          user <- archive.getUser(c)
        } yield assertTrue(user == Some(testUser), res == c)
      } +
      test("saveBlob with no overwrite - fail") {
        for {
          archive <- ZIO.service[UserArchive]
          fiber <- archive.saveUser(testUser2, false).fork
          _ <- TestClock.adjust(Duration.fromMillis(10))
          res <- assertZIO(fiber.await)(fails(isSubtype[ResourceConflictException](anything)))
        } yield res
      } +
      test("saveBlob with overwrite - success") {
        for {
          archive <- ZIO.service[UserArchive]
          fiber <- archive.saveUser(testUser2, true).fork
          _ <- TestClock.adjust(Duration.fromMillis(10))
          res <- fiber.join
          user <- archive.getUser(c)
        } yield assertTrue(user == Some(testUser2), res == c)
      } +
      test("getUser - success") {
        for {
          archive <- ZIO.service[UserArchive]
          user <- archive.getUser(c)
        } yield assertTrue(user == Some(testUser2))
      } +
      test("deleteBlob - success") {
        for {
          archive <- ZIO.service[UserArchive]
          _         <- archive.deleteUser(testUser.c)
          resDelete <- ZIO.succeed(true)  //  TODO: fix this hack; Giulio Cesare 26-02-2024
          resGet <- archive.getUser(c).map(_.isDefined)
        } yield assertTrue(resDelete, !resGet)
      } +
      test("deleteBlob - fail - not present") {
        for {
          archive <- ZIO.service[UserArchive]
          res <- assertZIO(archive.deleteUser(testUser2.c).exit)(fails(isSubtype[ResourceNotFoundException](anything)))
        } yield res
      }
  ).provideSomeLayerShared(environment) @@
    TestAspect.sequential @@
    // TestAspect.beforeAll(ZIO.succeed(FileSystem.deleteAllFiles(userBasePath.toFile().nn))) @@
    TestAspect.beforeAll(TestUtilities.deleteFilesInFolder(userBasePath)) @@
    // TestAspect.afterAll(ZIO.succeed(FileSystem.deleteAllFiles(userBasePath.toFile().nn)))
    TestAspect.afterAll (TestUtilities.deleteFilesInFolder(userBasePath))
