package is.clipperz.backend.services

import is.clipperz.backend.data.HexString
import is.clipperz.backend.functions.ByteArrays
import is.clipperz.backend.functions.crypto.HashFunction

import zio.{ ZIO, Layer, ZLayer, Tag, Task }
import zio.internal.stacktracer.Tracer
import zio.json.{ JsonDecoder, JsonEncoder, DeriveJsonDecoder, DeriveJsonEncoder, EncoderOps }

type TollCost = Int // bit
type TollReceipt = HexString
type Toll = HexString

case class TollChallenge(toll: Toll, cost: TollCost)

object TollChallenge:
  implicit val decoder: JsonDecoder[TollChallenge] = DeriveJsonDecoder.gen[TollChallenge]
  implicit val encoder: JsonEncoder[TollChallenge] = DeriveJsonEncoder.gen[TollChallenge]

enum ChallengeType:
  case CONNECT, REGISTER, MESSAGE, SHARE

val tollByteSize = 32

def byteToBinary(b: Byte): String =
  String.format("%8s", Integer.toBinaryString(b & 0xff)).nn.replace(' ', '0').nn

trait TollManager:
  def getToll(cost: TollCost): Task[TollChallenge]
  def verifyToll(challenge: TollChallenge, receipt: TollReceipt): Task[Boolean]
  def getChallengeCost(challengeType: ChallengeType): TollCost

object TollManager:
  val tollHeader = "clipperz-hashcash-tollchallenge"
  val tollCostHeader = "clipperz-hashcash-tollcost"
  val tollReceiptHeader = "clipperz-hashcash-tollreceipt"

  val tollChallengeContentKey = "tollChallenge"

  case class DefaultTollManager(prng: PRNG) extends TollManager:
    override def getToll(cost: TollCost): Task[TollChallenge] =
      if cost >= 0 then
        prng
          .nextBytes(tollByteSize)
          .map(bytes => TollChallenge(HexString.bytesToHex(bytes), cost))
      else ZIO.fail(new IllegalArgumentException("Toll cost can not be negative"))

    override def verifyToll(challenge: TollChallenge, receipt: TollReceipt): Task[Boolean] =
      if challenge.cost >= 0 then
        val binaryToll = challenge.toll.toByteArray.map(byteToBinary).mkString
        ByteArrays
          .hashOfArrays(HashFunction.hashSHA256, receipt.toByteArray) // receipt hash
          .map(hash => hash.map(byteToBinary).mkString)
          .map(binaryHash => binaryHash.take(challenge.cost) == binaryToll.take(challenge.cost))
      else ZIO.fail(new IllegalArgumentException("Invalid challenge cost"))

    override def getChallengeCost(challengeType: ChallengeType): TollCost =
      challengeType match
        case ChallengeType.CONNECT  => 3
        case ChallengeType.REGISTER => 4
        case ChallengeType.MESSAGE  => 2
        case ChallengeType.SHARE    => 3 

  val live: ZLayer[PRNG, Throwable, TollManager] =
    ZLayer.scoped(
      for {
        prng <- ZIO.service[PRNG]
      } yield DefaultTollManager(prng)
    )

  def computeReceipt(prng: PRNG, tollManager: TollManager)(challenge: TollChallenge): Task[TollReceipt] =
    prng
      .nextBytes(tollByteSize)
      .map(HexString.bytesToHex(_))
      .flatMap(receipt =>
        ZIO
          .ifZIO(tollManager.verifyToll(challenge, receipt))
          .apply(
            ZIO.succeed(receipt),
            computeReceipt(prng, tollManager)(challenge),
          )
      )
