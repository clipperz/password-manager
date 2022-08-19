package is.clipperz.backend.services

import zio.{ ZIO, Task, Layer, ZLayer, Tag }
import zio.internal.stacktracer.Tracer

trait PRNG:
  def nextBytes(size: Int): Task[Array[Byte]]

object PRNG:
  case class BasicPRNG() extends PRNG:
    override def nextBytes(size: Int): Task[Array[Byte]] =
      ZIO.attempt(Array.fill(size)((scala.util.Random.nextInt(256) - 128).toByte))

  val live: Layer[Nothing, PRNG] =
    ZLayer.succeed[PRNG](new BasicPRNG)(Tag[PRNG], Tracer.newTrace)
