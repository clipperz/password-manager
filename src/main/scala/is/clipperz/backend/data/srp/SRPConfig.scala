package is.clipperz.backend.data.srp

import is.clipperz.backend.functions.crypto.{ HashFunction, KeyDerivationFunction }

case class SRPConfigV6a(
    val group: SRPGroup,
    val k: BigInt,
    val hash: HashFunction,
    val keyDerivationFunction: KeyDerivationFunction,
  )
