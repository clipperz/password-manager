package is.clipperz.backend

import is.clipperz.backend.services.{ tollByteSize, PRNG }
import zio.test.Gen
import zio.ZIO

object TestUtilities:
  def getBytesGen(prng: PRNG): Gen[Any, Array[Byte]] = 
    Gen.fromZIO(prng
                .nextBytes(tollByteSize)
                .catchAll(_ => ZIO.succeed(Array.emptyByteArray)))
