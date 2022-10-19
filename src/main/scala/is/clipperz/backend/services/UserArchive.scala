package is.clipperz.backend.services

import java.nio.charset.StandardCharsets
import java.nio.file.Path
import zio.{ ZIO, ZLayer, Tag, Task, Chunk }
import zio.json.{ JsonDecoder, JsonEncoder, DeriveJsonDecoder, DeriveJsonEncoder, EncoderOps }
import zio.stream.{ ZSink, ZStream }
import is.clipperz.backend.data.HexString
import is.clipperz.backend.functions.fromStream

// ============================================================================

case class UserCard(
    c: HexString,
    s: HexString,
    v: HexString,
    srpVersion: String,
    masterKeyEncodingVersion: String,
    masterKeyContent: HexString,
  )

object UserCard:
  implicit val decoder: JsonDecoder[UserCard] = DeriveJsonDecoder.gen[UserCard]
  implicit val encoder: JsonEncoder[UserCard] = DeriveJsonEncoder.gen[UserCard]

// ============================================================================

trait UserArchive:
  def getUser(username: HexString): Task[Option[UserCard]]
  def saveUser(user: UserCard, overwrite: Boolean): Task[HexString]
  def deleteUser(username: HexString): Task[Boolean]

object UserArchive:
  case class FileSystemUserArchive(keyBlobArchive: KeyBlobArchive) extends UserArchive:
    override def getUser(username: HexString): Task[Option[UserCard]] =
      keyBlobArchive
        .getBlob(username.toString)
        .flatMap(fromStream[UserCard])
        .map(cr => Some(cr))
        .catchAll(_ => ZIO.succeed(None))

    override def saveUser(userCard: UserCard, overwrite: Boolean): Task[HexString] =
      def saveUserCard(userCard: UserCard): Task[HexString] =
        keyBlobArchive
        .saveBlob (
          userCard.c.toString,
          ZStream.fromChunks(Chunk.fromArray(userCard.toJson.getBytes(StandardCharsets.UTF_8).nn)),
        ).map(_ => userCard.c)
      getUser(userCard.c).flatMap(optionalUser => optionalUser match
        case Some(user) => if (overwrite) saveUserCard(userCard) else ZIO.fail(new Exception("Cannot save this user"))
        case None => saveUserCard(userCard)
      )

    override def deleteUser(username: HexString): Task[Boolean] =
      keyBlobArchive.deleteBlob(username.toString)

  def fs(basePath: Path, levels: Int): ZLayer[Any, Throwable, UserArchive] =
    val keyBlobArchive = new KeyBlobArchive.FileSystemKeyBlobArchive(basePath, levels);
    ZLayer.succeed[UserArchive](new FileSystemUserArchive(keyBlobArchive))
