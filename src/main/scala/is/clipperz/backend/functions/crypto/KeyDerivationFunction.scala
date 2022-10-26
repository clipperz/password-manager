package is.clipperz.backend.functions.crypto

import zio.{ Task }
import zio.stream.{ ZStream }

type KeyDerivationFunction = (Array[Byte], Array[Byte]) => Task[Array[Byte]]

object KeyDerivationFunction:

  val kdfSHA256: KeyDerivationFunction = (salt: Array[Byte], password: Array[Byte]) => {
    HashFunction.hashSHA256(ZStream.fromIterable(salt ++ password))
  }

  val kdfSHA1: KeyDerivationFunction = (salt: Array[Byte], password: Array[Byte]) => {
    HashFunction.hashSHA1(ZStream.fromIterable(salt ++ password))
  }

