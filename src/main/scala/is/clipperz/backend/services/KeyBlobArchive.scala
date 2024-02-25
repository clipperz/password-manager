package is.clipperz.backend.services

import is.clipperz.backend.exceptions.{
    NonWritableArchiveException,
    NonReadableArchiveException,
    ResourceNotFoundException,
    EmptyContentException,
}
import is.clipperz.backend.middleware.scheduledFileSystemMetricsCollection

import java.io.{ File, FileNotFoundException }
import java.nio.file.{ Files, Path }

import zio.{ Duration, Task, ZIO }
import zio.stream.{ ZSink, ZStream }

// ============================================================================

type Key = String

trait KeyBlobArchive:
    def saveBlob             (key: Key, content:  ZStream[Any, Throwable, Byte]): Task[Unit]
    def saveBlobWithMetadata (key: Key, content:  ZStream[Any, Throwable, Byte], metadata: ZStream[Any, Throwable, Byte]): Task[Unit]
    def getBlob              (key: Key): Task[ZStream[Any, Throwable, Byte]]
    def getMetadata          (key: Key): Task[ZStream[Any, Throwable, Byte]]
    def deleteBlob           (key: Key): Task[Unit]

object KeyBlobArchive:
    val WAIT_TIME = 100

    val metadataExtension = Some("metadata")
    def pathWithOptionalExtension (path: Path, extension: Option[String]): Path = extension.map(x => path.resolveSibling(path.getFileName().toString() + "." + x).nn).getOrElse(path)

    class FileSystemKeyBlobArchive private (basePath: Path, levels: Int) extends KeyBlobArchive:

        private def getContent (key: Key, extension: Option[String]): Task[ZStream[Any, Throwable, Byte]] =
            getBlobPath(key, false)
            .map(path => pathWithOptionalExtension(path, extension))
            .map(path =>
                if Files.exists(path)
                then ZIO.succeed(ZStream.fromPath(path))
                else ZIO.fail(new ResourceNotFoundException("Blob metadata not found"))
            )
            .getOrElse(ZIO.fail(new ResourceNotFoundException("Blob metadata not found")))

        override def getBlob (key: Key): Task[ZStream[Any, Throwable, Byte]] = getContent(key, None)

        override def getMetadata(key: Key): Task[ZStream[Any, Throwable, Byte]] = getContent(key, metadataExtension)

        private def saveData (key: Key, extension: Option[String], content: ZStream[Any, Throwable, Byte]): Task[Unit] = ZIO
            .fromOption(getBlobPath(key, true))
            .map(path => pathWithOptionalExtension(path, extension))
            .mapError(_ => new NonWritableArchiveException("Could not create blob file"))
            .flatMap(path => content
                .timeoutFail(new EmptyContentException)(Duration.fromMillis(WAIT_TIME))
                .run(ZSink.fromPath(path))
                .map(_ => ())
            )
            .catchSome:
                case ex: EmptyContentException => ZIO.fail(ex)
                case ex: NonReadableArchiveException => ZIO.fail(ex)
                case ex => ZIO.fail(new NonWritableArchiveException(s"${ex}"))

        private  def saveMetadata (key: Key, metadata: ZStream[Any, Throwable, Byte]): Task[Unit]   = saveData (key, metadataExtension, metadata)
                
        override def saveBlob (key: Key, content:  ZStream[Any, Throwable, Byte]): Task[Unit] =
            saveData(key, None, content)

        override def saveBlobWithMetadata (key: Key, content: ZStream[Any, Throwable, Byte], metadata: ZStream[Any, Throwable, Byte]): Task[Unit] =
            saveBlob(key, content) <&> saveMetadata(key, metadata)


        override def deleteBlob (key: Key): Task[Unit] =
            ZIO.attempt:
                getBlobPath(key, false)
                    .map(path =>
                        Files.deleteIfExists(path)
                        Files.deleteIfExists(pathWithOptionalExtension(path, metadataExtension))
                    )
                    .get
                // TODO: delete empty folder?
            .foldZIO(err => ZIO.fail(new NonWritableArchiveException(err.toString())), _ => ZIO.succeed(()))

        private def getBlobFile (key: Key): File =
            val piecesLength: Int = key.length / levels
            val pieces: IndexedSeq[String | Null] =
                for (i <- 0 to levels - 1)
                yield key.substring(i * piecesLength, i * piecesLength + piecesLength)
            val subPathString: String = pieces.mkString("/")
            basePath.resolve(subPathString).nn.toFile().nn

        private def getBlobPath (key: Key, createFolders: Boolean): Option[Path] =
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

    object FileSystemKeyBlobArchive:
        def apply (
            basePath: Path,
            levels: Int,
            requireExistingPath: Boolean = true,
        ): Task[FileSystemKeyBlobArchive] =
            if (Files.exists(basePath) && Files.isDirectory(basePath)) || !requireExistingPath
            then
                scheduledFileSystemMetricsCollection(basePath).forkDaemon
                *>
                ZIO.succeed(new FileSystemKeyBlobArchive(basePath, levels))
            else
                ZIO.fail(new IllegalArgumentException("Base path does not exist"))
