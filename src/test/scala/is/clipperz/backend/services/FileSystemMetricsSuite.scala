package is.clipperz.backend.services

import zio.test.ZIOSpecDefault
import zio.Scope
import zio.test.Spec
import zio.test.{ ZIOSpecDefault, assertTrue, assert, assertCompletes, assertZIO, TestAspect }
import java.nio.file.FileSystems

import is.clipperz.backend.functions.collectFileSystemMetrics
import java.nio.file.Files
import zio.ZIO

object FileSystemMetricsSuite extends ZIOSpecDefault:

  def spec = suite("FileSystemMetrics")(
    test("countFileNumber - folder with one file") {
      for {
        result <- collectFileSystemMetrics(FileSystems.getDefault().nn.getPath("./src/test/resources/blobs").nn)
      } yield assertTrue(result._1 == 1, result._2 == 1018269)
    } + 
    test("countFileNumber - single nested folders") {
      for {
        result <- collectFileSystemMetrics(FileSystems.getDefault().nn.getPath("./src/test/resources/sizeTest/singleNestedFolders").nn)
      } yield assertTrue(result._1 == 15, result._2 == 2304)
    } + 
    test("countFileNumber - multiple nested folders") {
      for {
        result <- collectFileSystemMetrics(FileSystems.getDefault().nn.getPath("./src/test/resources/sizeTest/multipleNestedFolders").nn)
      } yield assertTrue(result._1 == 23, result._2 == 3376)
    }
  )

