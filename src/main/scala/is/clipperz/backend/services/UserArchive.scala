package is.clipperz.backend.services

import java.nio.charset.StandardCharsets
import java.nio.file.Path
import zio.{ ZIO, ZLayer, Tag, Task, Chunk }
import zio.json.{ JsonDecoder, JsonEncoder, DeriveJsonDecoder, DeriveJsonEncoder, EncoderOps }
import zio.stream.{ ZSink, ZStream }
import is.clipperz.backend.data.HexString
import is.clipperz.backend.functions.fromStream
import is.clipperz.backend.exceptions.ResourceNotFoundException
import is.clipperz.backend.exceptions.ResourceConflictException
import is.clipperz.backend.exceptions.BadRequestException

// ============================================================================

case class RequestUserCard(
    c: HexString,
    s: HexString,
    v: HexString,
    srpVersion: String,
    originMasterKey: Option[HexString],
    masterKey: (HexString, String)
  )

object RequestUserCard:
  implicit val decoder: JsonDecoder[RequestUserCard] = DeriveJsonDecoder.gen[RequestUserCard]
  implicit val encoder: JsonEncoder[RequestUserCard] = DeriveJsonEncoder.gen[RequestUserCard]

case class RemoteUserCard(
    c: HexString,
    s: HexString,
    v: HexString,
    srpVersion: String,
    masterKey: (HexString, String)
  )

object RemoteUserCard:
  implicit val decoder: JsonDecoder[RemoteUserCard] = DeriveJsonDecoder.gen[RemoteUserCard]
  implicit val encoder: JsonEncoder[RemoteUserCard] = DeriveJsonEncoder.gen[RemoteUserCard]

def remoteFromRequest(requestUserCard : RequestUserCard): RemoteUserCard =
  RemoteUserCard(
    requestUserCard.c,
    requestUserCard.s,
    requestUserCard.v,
    requestUserCard.srpVersion,
    requestUserCard.masterKey
  )

case class UserCard(
    originMasterKey: HexString,
    masterKey: (HexString, String)
  )

object UserCard:
  implicit val decoder: JsonDecoder[UserCard] = DeriveJsonDecoder.gen[UserCard]

// ============================================================================

trait UserArchive:
  def getUser(username: HexString): Task[Option[RemoteUserCard]]
  def saveUser(user: RemoteUserCard, overwrite: Boolean): Task[HexString]
  def deleteUser(c: HexString): Task[Boolean]

object UserArchive:
  case class FileSystemUserArchive(keyBlobArchive: KeyBlobArchive) extends UserArchive:
    override def getUser(username: HexString): Task[Option[RemoteUserCard]] =
        keyBlobArchive
        .getBlob(username.toString)
        .flatMap(fromStream[RemoteUserCard](_).map(Some.apply))
        .catchSome:
          case ex: ResourceNotFoundException => ZIO.succeed(None)
          case ex => ZIO.fail(ex)

    override def saveUser(userCard: RemoteUserCard, overwrite: Boolean): Task[HexString] =
      def saveUserCard(userCard: RemoteUserCard): Task[HexString] =
        keyBlobArchive
          .saveBlob(
            userCard.c.toString,
            ZStream.fromChunks(Chunk.fromArray(userCard.toJson.getBytes(StandardCharsets.UTF_8).nn)),
          )
          .map(_ => userCard.c)

      this.getUser(userCard.c).flatMap(optional => if optional.isDefined
        then (if (overwrite) 
                then saveUserCard(userCard)
                else ZIO.fail(new ResourceConflictException("User already present")))
        else saveUserCard(userCard)
      )

    override def deleteUser(c: HexString): Task[Boolean] =
      this
        .getUser(c)
        .flatMap(optional => if optional.isDefined
          then keyBlobArchive.deleteBlob(c.toString)
          else ZIO.fail(new ResourceNotFoundException("User does not exist"))
        )

  def fs(
      basePath: Path,
      levels: Int,
      requireExistingPath: Boolean = true,
  ): ZLayer[Any, Throwable, UserArchive] =
    ZLayer.fromZIO[Any, Throwable, UserArchive](
      KeyBlobArchive.FileSystemKeyBlobArchive(basePath, levels, requireExistingPath)
        .map(new FileSystemUserArchive(_))
    )
