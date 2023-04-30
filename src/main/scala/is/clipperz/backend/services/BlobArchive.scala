package is.clipperz.backend.services

import java.io.{ File, FileOutputStream, IOException }
import java.nio.file.Path
import java.security.MessageDigest
import zio.{ ZIO, ZLayer, Task, Chunk }
import zio.stream.{ ZStream, ZSink }
import zio.json.{ JsonDecoder, JsonEncoder, DeriveJsonDecoder, DeriveJsonEncoder }
import is.clipperz.backend.data.HexString
import is.clipperz.backend.data.HexString.bytesToHex
import is.clipperz.backend.functions.crypto.HashFunction
import is.clipperz.backend.exceptions.EmptyContentException
import zio.Duration
import java.io.FileNotFoundException
import is.clipperz.backend.exceptions.NonWritableArchiveException
import is.clipperz.backend.exceptions.BadRequestException

// ----------------------------------------------------------------------------

type BlobHash = HexString

case class SaveBlobData(
    data: HexString,
    hash: BlobHash,
  )

object SaveBlobData:
  implicit val decoder: JsonDecoder[SaveBlobData] = DeriveJsonDecoder.gen[SaveBlobData]
  implicit val encoder: JsonEncoder[SaveBlobData] = DeriveJsonEncoder.gen[SaveBlobData]

// ----------------------------------------------------------------------------

trait BlobArchive:
  def getBlob(hash: BlobHash): Task[ZStream[Any, Throwable, Byte]]
  def saveBlob(key: BlobHash, content: ZStream[Any, Throwable, Byte]): Task[BlobHash]
  def deleteBlob(key: BlobHash, content: ZStream[Any, Throwable, Byte]): Task[Boolean]

object BlobArchive:
  val WAIT_TIME = 100

  case class FileSystemBlobArchive(keyBlobArchive: KeyBlobArchive, tmpDir: Path) extends BlobArchive:
    override def getBlob(hash: BlobHash): Task[ZStream[Any, Throwable, Byte]] =
      keyBlobArchive.getBlob(hash.toString)

    override def saveBlob(key: BlobHash, content: ZStream[Any, Throwable, Byte]): Task[BlobHash] =
      val tmpFile = File.createTempFile("pre", "suff", tmpDir.toFile())
      ZIO
        .scoped {
          content
            .timeoutFail(new EmptyContentException)(Duration.fromMillis(WAIT_TIME))
            .tapSink(ZSink.fromOutputStream(new FileOutputStream(tmpFile)))
            .run(ZSink.digest(MessageDigest.getInstance("SHA-256").nn))
            .map((chunk: Chunk[Byte]) => HexString.bytesToHex(chunk.toArray))
        }
        .flatMap { hash =>
          if (hash == key)
            ZIO.scoped {
              keyBlobArchive
                .saveBlob(hash.toString, ZStream.fromPath(tmpFile.nn.toPath().nn))
                .map(_ => tmpFile.nn.delete())
                .map(_ => hash)
            }
          else
            ZIO.fail(new BadRequestException(s"Hash of content does not match with hash in request"))
        }
        .catchSome {
          case ex: FileNotFoundException =>
            val str: String =
              if ex.getMessage() == null then "The temporary file or the blob could not be saved" else ex.getMessage().nn
            ZIO.fail(new NonWritableArchiveException(str))
          case ex: BadRequestException => ZIO.fail(ex)
          case ex: EmptyContentException => ZIO.fail(ex)
          case ex => ZIO.fail(new NonWritableArchiveException(s"${ex}"))
        }

    override def deleteBlob(key: BlobHash, content: ZStream[Any, Throwable, Byte]): Task[Boolean] =
      ZIO.scoped {
        HashFunction
          .hashSHA256(content)
          .map(hash => bytesToHex(hash))
          .flatMap(hash =>
            if key == hash then
              keyBlobArchive.deleteBlob(hash.toString)
            else
              ZIO.fail(new BadRequestException(s"Different hashes: computed = ${hash}, passed = ${key}"))
          )
      }

  // . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  def initializeBlobArchive(basePath: Path): Task[Unit] =
    ZIO.attempt {
      val file = basePath.toFile()
      val tempFolderSuccessfullyCreated: Boolean =
        (file match
          case null => None
          case _ => Some(file)
        ).map(p => if p.exists() then true else p.mkdirs())
          .getOrElse(false)
      if (tempFolderSuccessfullyCreated == false)
        throw new IOException("Failed initialization of temporary blob directory")
    }

  def fs(
      basePath: Path,
      levels: Int,
      requireExistingPath: Boolean = true,
    ): ZLayer[Any, Throwable, BlobArchive] =
    val keyBlobArchive = KeyBlobArchive.FileSystemKeyBlobArchive(basePath, levels, requireExistingPath)
    val baseTmpPath = basePath.resolve("tmp").nn

    ZLayer.scoped(
      initializeBlobArchive(baseTmpPath)
        .map(_ => FileSystemBlobArchive(keyBlobArchive, baseTmpPath))
    )
