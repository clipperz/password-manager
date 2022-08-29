package is.clipperz.backend.services

import zio.{ ZIO, Layer, ZLayer, Tag, Task }
import zio.internal.stacktracer.Tracer

import is.clipperz.backend.data.HexString
import is.clipperz.backend.functions.ByteArrays
import is.clipperz.backend.functions.crypto.HashFunction

type TollCost = Int // bit
type TollReceipt = HexString
type Toll = HexString
case class TollChallenge(toll: Toll, cost: TollCost)

enum ChallengeType: 
  case CONNECT, REGISTER, MESSAGE

val tollByteSize = 64

def byteToBinary(b: Byte): String =
  String.format("%4s", b.toInt.toBinaryString).nn.replace(' ', '0').nn

trait TollManager:
  def getToll(cost: TollCost): Task[TollChallenge]
  def verifyToll(challenge: TollChallenge, receipt: TollReceipt): Task[Boolean]
  def getChallengeCost(challengeType: ChallengeType): TollCost

object TollManager:
  val tollHeader        = "clipperz-HashCash-TollChallenge"
  val tollCostHeader    = "clipperz-HashCash-TollCost"
  val tollReceiptHeader = "clipperz-HashCash-TollReceipt"

  case class DefaultTollManager(prng: PRNG) extends TollManager:
    override def getToll(cost: TollCost): Task[TollChallenge] =
      prng.nextBytes(tollByteSize)
      .map(bytes => TollChallenge(HexString.bytesToHex(bytes), cost))
      
    override def verifyToll(challenge: TollChallenge, receipt: TollReceipt): Task[Boolean] =
      val toll = challenge.toll.toByteArray.map(byteToBinary).mkString
      ByteArrays.hashOfArrays(HashFunction.hashSHA256, receipt.toByteArray) // receipt hash
        .map(hash => hash.map(byteToBinary).mkString) // get binary string
        .map(s => s.take(challenge.cost) == toll.take(challenge.cost)) // check if equal bits are more than the cost of the challenge

    override def getChallengeCost(challengeType: ChallengeType): TollCost =
      challengeType match
        case ChallengeType.CONNECT  => 5
        case ChallengeType.REGISTER => 5
        case ChallengeType.MESSAGE  => 2

  val live: ZLayer[PRNG, Throwable, TollManager] =
    ZLayer.scoped(
      for {
        prng <- ZIO.service[PRNG]
      } yield DefaultTollManager(prng)
    )
