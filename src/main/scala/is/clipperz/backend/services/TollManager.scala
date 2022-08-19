package is.clipperz.backend.services

import zio.{ ZIO, Layer, ZLayer, Tag, Task }
import zio.internal.stacktracer.Tracer

type TollCost = Int
type TollContent = String
type TollChallenge = String
case class Toll(toll: TollChallenge, cost: TollCost)

trait TollManager:
  def getToll(cost: TollCost): Task[Toll]
  def verifyToll(challenge: TollChallenge, toll: TollContent): Task[Boolean]

object TollManager:
  case class EmptyTollManager(/* */ ) extends TollManager:
    override def getToll(cost: TollCost): Task[Toll] = ZIO.succeed(Toll("--------", 0))
    override def verifyToll(challenge: TollChallenge, toll: TollContent): Task[Boolean] =
      ZIO.succeed(false)

  val live: Layer[Nothing, TollManager] =
    ZLayer.succeed[TollManager](new EmptyTollManager)(Tag[TollManager], Tracer.newTrace)
