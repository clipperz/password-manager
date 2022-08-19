package is.clipperz.backend.functions

import is.clipperz.backend.data.HexString.{ bytesToHex, bigIntToHex }

object Conversions:
  def bytesToBigInt(bytes: Array[Byte]): BigInt =
    bytesToHex(bytes).toBigInt

  def bigIntToBytes(n: BigInt): Array[Byte] =
    bigIntToHex(n).toByteArray
