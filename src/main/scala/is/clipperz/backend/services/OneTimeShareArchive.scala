package is.clipperz.backend.services

import java.io.{ File, FileOutputStream, IOException }
import java.nio.file.Path
import java.security.MessageDigest
import zio.{ ZIO, ZLayer, Task, Chunk }
import zio.stream.{ ZStream, ZSink }
import zio.json.{ JsonDecoder, JsonEncoder, DeriveJsonDecoder, DeriveJsonEncoder }
import is.clipperz.backend.data.HexString
import is.clipperz.backend.data.HexString.{ bytesToHex }
import is.clipperz.backend.functions.crypto.HashFunction
import is.clipperz.backend.exceptions.EmptyContentException
import zio.Duration
import java.io.FileNotFoundException
import is.clipperz.backend.exceptions.NonWritableArchiveException
import is.clipperz.backend.exceptions.BadRequestException
import java.util.UUID

// ----------------------------------------------------------------------------

type SecretId = String

// ----------------------------------------------------------------------------

trait OneTimeShareArchive:
  def getSecret(id: SecretId): Task[ZStream[Any, Throwable, Byte]]
  def saveSecret(content: ZStream[Any, Throwable, Byte]): Task[SecretId]
  def deleteSecret(id: SecretId): Task[Boolean]

object OneTimeShareArchive:
  val WAIT_TIME = 100

  case class FileSystemOneTimeShareArchive(keyBlobArchive: KeyBlobArchive) extends OneTimeShareArchive:
    override def getSecret(id: SecretId): Task[ZStream[Any, Throwable, Byte]] =
      keyBlobArchive.getBlob(id)
    
    override def deleteSecret(id: SecretId): Task[Boolean] = 
      keyBlobArchive.deleteBlob(id)

    override def saveSecret(content: ZStream[Any, Throwable, Byte]): Task[SecretId] =
      val id = UUID.randomUUID().nn.toString();
      ZIO
        .scoped {
          keyBlobArchive
            .saveBlob(id, content)
            .map(_ => id)
        }
        .catchSome {
          case ex: FileNotFoundException =>
            val str: String =
              if ex.getMessage() == null then "The temporary file or the secret could not be saved" else ex.getMessage().nn
            ZIO.fail(new NonWritableArchiveException(str))
          case ex: BadRequestException => ZIO.fail(ex)
          case ex: EmptyContentException => ZIO.fail(ex)
          case ex => ZIO.fail(new NonWritableArchiveException(s"${ex}"))
        }

  // . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  def initializeOneTimeShareArchive(basePath: Path): Task[Unit] =
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
    ): ZLayer[Any, Throwable, OneTimeShareArchive] =
    val keyBlobArchive = KeyBlobArchive.FileSystemKeyBlobArchive(basePath, levels, requireExistingPath)
    ZLayer.succeed[OneTimeShareArchive](new FileSystemOneTimeShareArchive(keyBlobArchive))
