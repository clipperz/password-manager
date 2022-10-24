package is.clipperz.backend.services

import java.io.{ File, FileNotFoundException }
import java.nio.file.{ Files, Path }
import zio.{ Task, ZIO }
import zio.stream.{ ZSink, ZStream }

import is.clipperz.backend.exceptions.{
  NonWritableArchiveException, 
  NonReadableArchiveException, 
  ResourceNotFoundException, 
  EmptyContentException
}
import zio.Duration

// ============================================================================

type Key = String

trait KeyBlobArchive:
  def getBlob(key: Key): Task[ZStream[Any, Throwable, Byte]]
  def saveBlob(key: Key, content: ZStream[Any, Throwable, Byte]): Task[Unit]
  def deleteBlob(key: Key): Task[Boolean]

object KeyBlobArchive:
  val WAIT_TIME = 1

  case class FileSystemKeyBlobArchive(basePath: Path, levels: Int) extends KeyBlobArchive:
    override def getBlob(key: Key): Task[ZStream[Any, Throwable, Byte]] =
      getBlobPath(key, false)
        .map(path => if (Files.exists(path)) then
            ZIO.succeed(ZStream.fromPath(path))
          else 
            ZIO.fail(new ResourceNotFoundException("Blob not found")))
        .getOrElse(ZIO.fail(new ResourceNotFoundException("Blob not found")))
        .catchSome {
          case ex : ResourceNotFoundException => ZIO.fail(ex)
          case ex => ZIO.fail(new NonReadableArchiveException(s"${ex}"))
        }

    override def saveBlob(key: Key, content: ZStream[Any, Throwable, Byte]): Task[Unit] =
        ZIO
          .fromOption(getBlobPath(key, true))
          .mapError(_ => new NonWritableArchiveException("Could not create blob file"))
          .flatMap(path => content
                        .timeoutFail(new EmptyContentException)(Duration.fromMillis(WAIT_TIME))
                        .run(ZSink.fromPath(path))
                        .map(_ => ()))
          .catchSome {
            case ex : EmptyContentException => ZIO.fail(ex)
            case ex : NonReadableArchiveException => ZIO.fail(ex)
            case ex => ZIO.fail(new NonWritableArchiveException(s"${ex}"))
          }

    override def deleteBlob(key: Key): Task[Boolean] =
      ZIO.attempt {
        getBlobPath(key, false)
          .map(path => Files.deleteIfExists(path))
          .get
          // TODO: delete empty folder?
      }.foldZIO(err => ZIO.fail(new NonWritableArchiveException(err.toString())), data => ZIO.succeed(data))

    private def getBlobFile(key: Key): File =
      val piecesLength: Int = key.length / levels
      val pieces: IndexedSeq[String | Null] =
        for (i <- 0 to levels - 1)
          yield key.substring(i * piecesLength, i * piecesLength + piecesLength)
      val subPathString: String = pieces.mkString("/")
      basePath.resolve(subPathString).nn.toFile().nn

    private def getBlobPath(key: Key, createFolders: Boolean): Option[Path] =
      val file = getBlobFile(key)
      val path = file.toPath
      val optionalPath = path match
        case null => None
        case _ => Some(path)

      if (optionalPath.isDefined && createFolders)
        if (!Files.exists(file.getParentFile.nn.toPath))
          file.getParentFile().nn.mkdirs()
        file.createNewFile()

      optionalPath
