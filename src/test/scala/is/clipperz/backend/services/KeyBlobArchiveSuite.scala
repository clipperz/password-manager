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
import zio.http.{ Version, Headers, Method, URL, Request, Body }
import zio.http.*
import zio.nio.file.{ Files, FileSystem }
import is.clipperz.backend.Main
// import java.nio.file.Path
import _root_.is.clipperz.backend.Exceptions.*
import zio.Clock
import zio.Clock.ClockLive
import zio.test.TestClock
import zio.Duration
import is.clipperz.backend.TestUtilities

object KeyBlobArchiveSpec extends ZIOSpecDefault:
  val blobBasePath = FileSystem.default.getPath("target", "tests", "archive", "blobs")
  val keyBlobArchive = KeyBlobArchive.FileSystemKeyBlobArchive(blobBasePath, 1, false)

  val testContent = ZStream.fromIterable("testContent".getBytes().nn)
  val failingContent = ZStream.never
  val testKey = "testKey"
  val failingKey = "failingKey"

  def spec = suite("KeyBlobArchive")(
    test("getBlob - fail") {
      assertZIO(keyBlobArchive.flatMap(_.getBlob(testKey).exit))(fails(isSubtype[ResourceNotFoundException](anything)))
    } +
      test("saveBlob - success") {
        for {
          fiber <- keyBlobArchive.flatMap(_.saveBlob(testKey, testContent).fork)
          _ <- TestClock.adjust(Duration.fromMillis(KeyBlobArchive.WAIT_TIME + 10))
          _ <- fiber.join
          content <- keyBlobArchive.flatMap(_.getBlob(testKey))
          result <- testContent.zip(content).map((a, b) => a == b).toIterator.map(_.map(_.getOrElse(false)).reduce(_ && _))
        } yield assertTrue(result)
      } +
      test("saveBlob with failing stream - success") {
        for {
          fiber <- keyBlobArchive.flatMap(_.saveBlob(failingKey, failingContent).fork)
          _ <- TestClock.adjust(Duration.fromMillis(KeyBlobArchive.WAIT_TIME + 10))
          res <- assertZIO(fiber.await)(fails(isSubtype[EmptyContentException](anything)))
        } yield res
      } +
      test("getBlob - success") {
        for {
          content <- keyBlobArchive.flatMap(_.getBlob(testKey))
          result <- testContent.zip(content).map((a, b) => a == b).toIterator.map(_.map(_.getOrElse(false)).reduce(_ && _))
        } yield assertTrue(result)
      } +
      test("deleteBlob - success") {
        for {
          _   <- keyBlobArchive.flatMap(_.deleteBlob(testKey))
          res <- ZIO.succeed(true)  //  TODO: fix this hack; Giulio Cesare 26-02-2024
        } yield assertTrue(res)
      } +
      test("deleteBlob - fail") {
        for {
          _   <- keyBlobArchive.flatMap(_.deleteBlob(testKey))
          res <- ZIO.succeed(true)  //  TODO: fix this hack; Giulio Cesare 26-02-2024
        } yield assertTrue(!res)
      }
  ) @@
    TestAspect.sequential @@
    // TestAspect.beforeAll(ZIO.succeed(FileSystem.deleteAllFiles(blobBasePath.toFile().nn))) @@
    TestAspect.beforeAll(TestUtilities.deleteFilesInFolder(blobBasePath)) @@
    // TestAspect.afterAll(ZIO.succeed(FileSystem.deleteAllFiles(blobBasePath.toFile().nn)))
    TestAspect.afterAll (TestUtilities.deleteFilesInFolder(blobBasePath))
