package is.clipperz.backend

import is.clipperz.backend.services.{ tollByteSize, PRNG }
import zio.test.Gen
import zio.{ZIO, Task }
import scala.collection.immutable.HashMap
import is.clipperz.backend.services.Session
import zio.nio.file.{ Files, Path }
import zio.stream.ZSink

object TestUtilities:
    def deleteFilesInFolder (path: Path): ZIO[Any, Nothing, Boolean] =
        Files.newDirectoryStream(path).mapZIO { p =>
            for {
                deletedInSubDirectory <- deleteFilesInFolder(p) .whenZIO(Files.isDirectory(p))  .map(_.getOrElse(false))
                deletedFile           <- Files.deleteIfExists(p).whenZIO(Files.isRegularFile(p)).map(_.getOrElse(false))
            } yield deletedInSubDirectory && deletedFile
        }
        .run(ZSink.collectAll)
        .map(_.toArray.foldLeft(true)((a, b) => a && b))
        .catchAll(_ => ZIO.succeed(false))


    def getBytesGen(prng: PRNG, size: Int): Gen[Any, Array[Byte]] =
        Gen.fromZIO(
        prng
            .nextBytes(size)
            .catchAll(_ => ZIO.succeed(Array.emptyByteArray))
        )
