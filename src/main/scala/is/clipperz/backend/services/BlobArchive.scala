package is.clipperz.backend.services

import is.clipperz.backend.data.HexString
import is.clipperz.backend.data.HexString.bytesToHex
import is.clipperz.backend.functions.crypto.HashFunction
import is.clipperz.backend.exceptions.{ EmptyContentException, NonWritableArchiveException, BadRequestException }

import java.io.{ File, FileNotFoundException, FileOutputStream, IOException }
import java.nio.file.Path
import java.security.MessageDigest

import zio.{ Chunk, Duration, ZIO, ZLayer, Task }
import zio.stream.{ ZStream, ZSink }
import zio.json.{ JsonDecoder, JsonEncoder, DeriveJsonDecoder, DeriveJsonEncoder }

// ----------------------------------------------------------------------------

type BlobHash = HexString

// ----------------------------------------------------------------------------

trait BlobArchive:
    def getBlob             (hash: BlobHash): Task[ZStream[Any, Throwable, Byte]]
    def getBlobIdentifier   (hash: BlobHash): Task[HexString]
    def saveBlob            (hash: BlobHash, identifier: HexString, content:  ZStream[Any, Throwable, Byte]): Task[BlobHash]
    def deleteBlob          (hash: BlobHash, identifier: HexString): Task[Unit]

object BlobArchive:
    val WAIT_TIME = 100

    case class FileSystemBlobArchive(keyBlobArchive: KeyBlobArchive, tmpDir: Path) extends BlobArchive:
        override def getBlob(hash: BlobHash): Task[ZStream[Any, Throwable, Byte]] =
            keyBlobArchive.getBlob(hash.toString)

        override def getBlobIdentifier(hash: BlobHash): Task[HexString] =
            keyBlobArchive
                .getMetadata(hash.toString)
                .flatMap(_.run(ZSink.collectAll[Byte]))
                .map(_.toArray)
                .map(bytesToHex(_))

        override def saveBlob(hash: BlobHash, identifier: HexString, content: ZStream[Any, Throwable, Byte]): Task[BlobHash] =
            val tmpFile = File.createTempFile("pre", "suff", tmpDir.toFile())
            ZIO.scoped:
                content
                    .timeoutFail(new EmptyContentException)(Duration.fromMillis(WAIT_TIME))
                    .tapSink(ZSink.fromOutputStream(new FileOutputStream(tmpFile)))
                    .run(ZSink.digest(MessageDigest.getInstance("SHA-256").nn))
                    .map((chunk: Chunk[Byte]) => HexString.bytesToHex(chunk.toArray))
            .flatMap { hash_ =>
                if (hash_ == hash)
                then ZIO.scoped:
                    keyBlobArchive
                        .saveBlobWithMetadata(hash.toString, ZStream.fromPath(tmpFile.nn.toPath().nn), ZStream.fromChunk(Chunk.fromArray(identifier.toByteArray)))
                        .map(_ => tmpFile.nn.delete())
                        .map(_ => hash)
                else ZIO.fail(new BadRequestException(s"Hash of content does not match with hash in request"))
            }
            .catchSome:
                case ex: FileNotFoundException =>
                    val str: String =
                        if ex.getMessage() == null
                        then "The temporary file or the blob could not be saved"
                        else ex.getMessage().nn
                    ZIO.fail(new NonWritableArchiveException(str))
                case ex: BadRequestException => ZIO.fail(ex)
                case ex: EmptyContentException => ZIO.fail(ex)
                case ex => ZIO.fail(new NonWritableArchiveException(s"${ex}"))

        override def deleteBlob(hash: BlobHash, identifier: HexString): Task[Unit] =
            ZIO.scoped:
                this.getBlobIdentifier(hash)
                    .flatMap(storedIdentifier =>
                        if storedIdentifier == identifier
                        then keyBlobArchive.deleteBlob(hash.toString)
                        else ZIO.fail(new BadRequestException(s"Wrong 'identifier' provided"))
                    )

            // ZIO.scoped:
            //     HashFunction
            //     .hashSHA256(content)
            //     .map(hash => bytesToHex(hash))
            //     .flatMap(hash =>
            //         if key == hash
            //         then keyBlobArchive.deleteBlob(hash.toString)
            //         else ZIO.fail(new BadRequestException(s"Different hashes: computed = ${hash}, passed = ${key}"))
            //     )

  // . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

    def initializeBlobArchive (basePath: Path): Task[Unit] =
        ZIO.attempt:
            val file = basePath.toFile()
            val tempFolderSuccessfullyCreated: Boolean = (file match
                case null   => None
                case _      => Some(file)
            )
            .map(p => if p.exists() then true else p.mkdirs())
            .getOrElse(false)
            if (tempFolderSuccessfullyCreated == false)
                throw new IOException("Failed initialization of temporary blob directory")

    def fs (
        basePath: Path,
        levels: Int,
        requireExistingPath: Boolean = true,
    ): ZLayer[Any, Throwable, BlobArchive] =
        val keyBlobArchive = KeyBlobArchive.FileSystemKeyBlobArchive(basePath, levels, requireExistingPath)
        val baseTmpPath = basePath.resolve("tmp").nn
        ZLayer.scoped(
            initializeBlobArchive(baseTmpPath)
            *>
            keyBlobArchive.map(FileSystemBlobArchive(_, baseTmpPath)
        )
)
