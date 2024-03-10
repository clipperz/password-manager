package is.clipperz.backend.services

import is.clipperz.backend.data.HexString
import is.clipperz.backend.data.HexString.{ bytesToHex, bigIntToHex }
import is.clipperz.backend.functions.Conversions.{ bigIntToBytes, bytesToBigInt }
import is.clipperz.backend.functions.SrpFunctions.SrpFunctionsV6a
import is.clipperz.backend.Exceptions.*

import zio.{ ZIO, ZLayer, Task }
import zio.json.{ JsonDecoder, JsonEncoder, DeriveJsonDecoder, DeriveJsonEncoder }
import zio.stream.ZStream

// ============================================================================

case class CardsSignupData(
    cardContent: HexString,
    cardReference: HexString,
    cardIdentifier: HexString
)

object CardsSignupData:
  implicit val decoder: JsonDecoder[CardsSignupData] = DeriveJsonDecoder.gen[CardsSignupData]
  implicit val encoder: JsonEncoder[CardsSignupData] = DeriveJsonEncoder.gen[CardsSignupData]


case class SignupData(
    user: RequestUserCard,

    userInfoReference: HexString,
    userInfoIdentifier: HexString,
    userInfoContent: HexString,

    indexCardReference: HexString,
    indexCardIdentifier: HexString,
    indexCardContent: HexString,

    cards: Array[CardsSignupData],
  )
object SignupData:
  implicit val decoder: JsonDecoder[SignupData] = DeriveJsonDecoder.gen[SignupData]
  implicit val encoder: JsonEncoder[SignupData] = DeriveJsonEncoder.gen[SignupData]

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

case class SRPStep1Data(c: HexString, aa: HexString)
object SRPStep1Data:
  implicit val decoder: JsonDecoder[SRPStep1Data] = DeriveJsonDecoder.gen[SRPStep1Data]
  implicit val encoder: JsonEncoder[SRPStep1Data] = DeriveJsonEncoder.gen[SRPStep1Data]

case class SRPStep1Response(s: HexString, bb: HexString)
object SRPStep1Response:
  implicit val decoder: JsonDecoder[SRPStep1Response] = DeriveJsonDecoder.gen[SRPStep1Response]
  implicit val encoder: JsonEncoder[SRPStep1Response] = DeriveJsonEncoder.gen[SRPStep1Response]

case class SRPStep2Data(m1: HexString)
object SRPStep2Data:
  implicit val decoder: JsonDecoder[SRPStep2Data] = DeriveJsonDecoder.gen[SRPStep2Data]
  implicit val encoder: JsonEncoder[SRPStep2Data] = DeriveJsonEncoder.gen[SRPStep2Data]

case class SRPStep2Response(m2: HexString, masterKey: (HexString, MasterKeyEncodingVersion))
object SRPStep2Response:
  implicit val decoder: JsonDecoder[SRPStep2Response] = DeriveJsonDecoder.gen[SRPStep2Response]
  implicit val encoder: JsonEncoder[SRPStep2Response] = DeriveJsonEncoder.gen[SRPStep2Response]

// ============================================================================

trait SrpManager:
  def srpStep1(step1Data: SRPStep1Data, session: Session): Task[(SRPStep1Response, Session)]
  def srpStep2(step2Data: SRPStep2Data, session: Session): Task[(SRPStep2Response, Session)]

object SrpManager:
  case class SrpManagerV6a(
      userArchive: UserArchive,
      prng: PRNG,
      srpFunctions: SrpFunctionsV6a,
    ) extends SrpManager:
    val newRandomValue = () => prng.nextBytes(64)
    val configuration = srpFunctions.configuration
    val nn = configuration.group.nn
    override def srpStep1(step1Data: SRPStep1Data, session: Session): Task[(SRPStep1Response, Session)] =
      userArchive
        .getUser(step1Data.c)
        .flatMap(optionalUser =>
          optionalUser match
            case Some(u) => ZIO.succeed(u)
            case None => ZIO.fail(new ResourceNotFoundException(s"user ${step1Data.c} does not exist"))
        )
        .zip(newRandomValue())
        .map { (userCard, randomValue) =>
          val v = userCard.v.toBigInt
          val s = userCard.s
          val b = bytesToHex(randomValue)
          val bb = bigIntToHex(srpFunctions.computeB(b.toBigInt, v))

          val newSessionContext =
            session + (("c", step1Data.c.toString)) + (("b", b.toString)) + (("B", bb.toString)) + (("A", step1Data.aa.toString))
          (SRPStep1Response(s = s, bb = bb), newSessionContext)
        }

    override def srpStep2(step2Data: SRPStep2Data, session: Session): Task[(SRPStep2Response, Session)] =
      val aa = HexString(session("A").get)
      val bb = HexString(session("B").get)
      val b = HexString(session("b").get)
      val c = HexString(session("c").get)
      val zioUser = userArchive.getUser(c).flatMap(u => ZIO.attempt(u.get))
      val zioK: Task[Array[Byte]] = for {
        u : Array[Byte] <- srpFunctions.computeU(aa.toByteArray, bb.toByteArray)
        user: RemoteUserCard <- zioUser
        v: BigInt <- ZIO.attempt(user.v.toBigInt)
        secret: BigInt <- ZIO.succeed(srpFunctions.computeSecretServer(aa.toBigInt, b.toBigInt, v, bytesToBigInt(u)))
        kk: Array[Byte] <- srpFunctions.computeK(secret)
        kk: Array[Byte] <- configuration.hash(ZStream.fromIterable(bigIntToBytes(secret)))
      } yield kk
      val zioM1: Task[Array[Byte]] = for {
        user: RemoteUserCard <- zioUser
        kk: Array[Byte] <- zioK
        m1: Array[Byte] <- srpFunctions.computeM1(user.c.toByteArray, user.s.toByteArray, aa.toByteArray, bb.toByteArray, kk)
      } yield m1

      zioM1.flatMap { m1 =>
        val m1Server = bytesToBigInt(m1)
        val m1Client = step2Data.m1.toBigInt
        if m1Server == m1Client then
          for {
            user: RemoteUserCard <- zioUser
            kk: Array[Byte] <- zioK
            m2: Array[Byte] <- srpFunctions.computeM2(aa.toByteArray, step2Data.m1.toByteArray, kk)
            result <- ZIO.succeed((SRPStep2Response(bytesToHex(m2), user.masterKey), session)) // TODO: what to put in session context
          } yield result
        else ZIO.fail(new BadRequestException(s"M1 is not correct => M1 SERVER ${bytesToHex(m1)} != M1 CLIENT ${step2Data.m1}"))
      }

  def v6a(): ZLayer[UserArchive & PRNG, Throwable, SrpManager] =
    val srpFunctions = new SrpFunctionsV6a()
    ZLayer.scoped(
      for {
        userArchive <- ZIO.service[UserArchive]
        prng <- ZIO.service[PRNG]
      } yield SrpManagerV6a(userArchive, prng, srpFunctions)
    )
