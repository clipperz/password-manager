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

case class UserCard(
    c: HexString,
    s: HexString,
    v: HexString,
    srpVersion: String,
    masterKeyEncodingVersion: String,
    masterKeyContent: HexString,
    preferences: UserPreferences
  )

object UserCard:
  implicit val decoder: JsonDecoder[UserCard] = DeriveJsonDecoder.gen[UserCard]
  implicit val encoder: JsonEncoder[UserCard] = DeriveJsonEncoder.gen[UserCard]

case class UserPreferences(passwordGeneratorSettings: PasswordGeneratorSettings, automaticLock: Option[Int])

object UserPreferences:
  implicit val decoder: JsonDecoder[UserPreferences] = DeriveJsonDecoder.gen[UserPreferences]
  implicit val encoder: JsonEncoder[UserPreferences] = DeriveJsonEncoder.gen[UserPreferences]

case class PasswordGeneratorSettings(
    length: Int,
    characterSets: List[(String, Boolean)],
    characters: String,
  )
// override def equals(x: Any): Boolean =
//   if x.isInstanceOf[PasswordGeneratorSettings] then
//     val that = x.asInstanceOf[PasswordGeneratorSettings]
//     that.length == this.length && that.characters == this.characters &&
//   else
//     false

object PasswordGeneratorSettings:
  implicit val decoder: JsonDecoder[PasswordGeneratorSettings] = DeriveJsonDecoder.gen[PasswordGeneratorSettings]
  implicit val encoder: JsonEncoder[PasswordGeneratorSettings] = DeriveJsonEncoder.gen[PasswordGeneratorSettings]

case class ModifyUserCard(
    c: HexString,
    oldUserCard: UserCard,
    newUserCard: UserCard,
  )

object ModifyUserCard:
  implicit val decoder: JsonDecoder[ModifyUserCard] = DeriveJsonDecoder.gen[ModifyUserCard]
  implicit val encoder: JsonEncoder[ModifyUserCard] = DeriveJsonEncoder.gen[ModifyUserCard]

// ============================================================================

trait UserArchive:
  def getUser(username: HexString): Task[Option[UserCard]]
  def saveUser(user: UserCard, overwrite: Boolean): Task[HexString]
  def deleteUser(user: UserCard): Task[Boolean]

object UserArchive:
  case class FileSystemUserArchive(keyBlobArchive: KeyBlobArchive) extends UserArchive:
    override def getUser(username: HexString): Task[Option[UserCard]] =
      keyBlobArchive
        .getBlob(username.toString)
        .flatMap(fromStream[UserCard])
        .map(cr => Some(cr))
        .catchSome {
          case ex: ResourceNotFoundException => ZIO.succeed(None)
          case ex => ZIO.fail(ex)
        }

    override def saveUser(userCard: UserCard, overwrite: Boolean): Task[HexString] =
      def saveUserCard(userCard: UserCard): Task[HexString] =
        keyBlobArchive
          .saveBlob(
            userCard.c.toString,
            ZStream.fromChunks(Chunk.fromArray(userCard.toJson.getBytes(StandardCharsets.UTF_8).nn)),
          )
          .map(_ => userCard.c)
      getUser(userCard.c).flatMap(optionalUser =>
        optionalUser match
          case Some(user) =>
            if (overwrite) saveUserCard(userCard) else ZIO.fail(new ResourceConflictException("User already present"))
          case None => saveUserCard(userCard)
      )

    override def deleteUser(user: UserCard): Task[Boolean] =
      this
        .getUser(user.c)
        .flatMap(o =>
          if o.isDefined then
            if o.contains(user) then keyBlobArchive.deleteBlob(user.c.toString)
            else ZIO.fail(new BadRequestException("User card is different than the one saved"))
          else ZIO.fail(new ResourceNotFoundException("User does not exist"))
        )

  def fs(
      basePath: Path,
      levels: Int,
      requireExistingPath: Boolean = true,
    ): ZLayer[Any, Throwable, UserArchive] =
    val keyBlobArchive = KeyBlobArchive.FileSystemKeyBlobArchive(basePath, levels, requireExistingPath);
    ZLayer.succeed[UserArchive](new FileSystemUserArchive(keyBlobArchive))
