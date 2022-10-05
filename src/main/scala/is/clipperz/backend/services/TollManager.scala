package is.clipperz.backend.services

import zio.{ ZIO, Layer, ZLayer, Tag, Task }
import zio.internal.stacktracer.Tracer
import zio.json.{ JsonDecoder, JsonEncoder, DeriveJsonDecoder, DeriveJsonEncoder, EncoderOps }

import is.clipperz.backend.data.HexString
import is.clipperz.backend.functions.ByteArrays
import is.clipperz.backend.functions.crypto.HashFunction


type TollCost = Int // bit
type TollReceipt = HexString
type Toll = HexString

case class TollChallenge(toll: Toll, cost: TollCost)

object TollChallenge:
  implicit val decoder: JsonDecoder[TollChallenge] = DeriveJsonDecoder.gen[TollChallenge]
  implicit val encoder: JsonEncoder[TollChallenge] = DeriveJsonEncoder.gen[TollChallenge]

enum ChallengeType: 
  case CONNECT, REGISTER, MESSAGE

val tollByteSize = 32

def byteToBinary(b: Byte): String =
  String.format("%8s", Integer.toBinaryString(b & 0xFF)).nn.replace(' ', '0').nn

trait TollManager:
  def getToll(cost: TollCost): Task[TollChallenge]
  def verifyToll(challenge: TollChallenge, receipt: TollReceipt): Task[Boolean]
  def getChallengeCost(challengeType: ChallengeType): TollCost

object TollManager:
  val tollHeader        = "clipperz-hashcash-tollchallenge"
  val tollCostHeader    = "clipperz-hashcash-tollcost"
  val tollReceiptHeader = "clipperz-hashcash-tollreceipt"

  val tollChallengeContentKey = "tollChallenge"

  case class DefaultTollManager(prng: PRNG) extends TollManager:
    override def getToll(cost: TollCost): Task[TollChallenge] =
      prng.nextBytes(tollByteSize)
      .map(bytes => TollChallenge(HexString.bytesToHex(bytes), cost))
      
    override def verifyToll(challenge: TollChallenge, receipt: TollReceipt): Task[Boolean] =
      val binaryToll = challenge.toll.toByteArray.map(byteToBinary).mkString
      ByteArrays.hashOfArrays(HashFunction.hashSHA256, receipt.toByteArray) // receipt hash
        .map(hash => hash.map(byteToBinary).mkString) // get binary string
        .map(binaryHash => 
          if binaryHash.take(challenge.cost) == binaryToll.take(challenge.cost) then
            true
          else
            // println(s" Toll -> ${binaryToll}; Receipt -> ${receipt}; Hash -> ${binaryHash}")
            false
        ) // check if equal bits are more than the cost of the challenge

    override def getChallengeCost(challengeType: ChallengeType): TollCost =
      challengeType match
        case ChallengeType.CONNECT  => 3
        case ChallengeType.REGISTER => 3
        case ChallengeType.MESSAGE  => 2

  val live: ZLayer[PRNG, Throwable, TollManager] =
    ZLayer.scoped(
      for {
        prng <- ZIO.service[PRNG]
      } yield DefaultTollManager(prng)
    )
