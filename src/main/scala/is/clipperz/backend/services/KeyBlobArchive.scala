package is.clipperz.backend.services

import java.io.{ File, FileNotFoundException }
import java.nio.file.{ Files, Path }
import zio.{ Task, ZIO }
import zio.stream.{ ZSink, ZStream }

import is.clipperz.backed.exceptions.{NonWritableArchiveException, NonReadableArchiveException, ResourceNotFoundException}

// ============================================================================

type Key = String

trait KeyBlobArchive:
  def getBlob(key: Key): Task[ZStream[Any, Throwable, Byte]]
  def saveBlob(key: Key, content: ZStream[Any, Throwable, Byte]): Task[Unit]
  def deleteBlob(key: Key): Task[Boolean]

object KeyBlobArchive:
  case class FileSystemKeyBlobArchive(basePath: Path, levels: Int) extends KeyBlobArchive:
    override def getBlob(key: Key): Task[ZStream[Any, Throwable, Byte]] =
      getBlobPath(key, false)
        .map(path => ZIO.succeed(ZStream.fromPath(path)))
        .getOrElse(ZIO.fail(new ResourceNotFoundException("Blob not found")))
        .catchSome {
          case ex : ResourceNotFoundException => ZIO.fail(ex)
          case ex => ZIO.fail(new NonReadableArchiveException(s"${ex}"))
        }

    override def saveBlob(key: Key, content: ZStream[Any, Throwable, Byte]): Task[Unit] =
      ZIO.scoped {
        getBlobPath(key, true)
          // .get // TODO: (DONE??) this takes time to create the file: needs a way to be sure the ZSink is applied after this has finished
          .map(path => content.run(ZSink.fromPath(path)).map(_ => ()))
          .get
      }.foldZIO(err => ZIO.fail(new NonWritableArchiveException(err.toString())), data => ZIO.succeed(data))

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
