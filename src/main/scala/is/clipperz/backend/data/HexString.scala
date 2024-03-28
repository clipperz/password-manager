package is.clipperz.backend.data

import zio.json.{ JsonDecoder, JsonEncoder }
import java.nio.charset.StandardCharsets

enum Base:
  case Dec, Hex

class HexString private (private val s: String):
    private val hexString: String = normalizeHex(
        if HexString.isHex(s)
        then s
        else HexString.hexEncode(s).toString
    )

    private def normalizeHex(s: String): String =
        val hexWithoutSpaces = s.filterNot(_.isWhitespace).toUpperCase.nn

        if (hexWithoutSpaces.length % 2 == 0)
        then hexWithoutSpaces
        else "0" + hexWithoutSpaces

    def toByteArray: Array[Byte] =
        this.hexString.sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)

    def toBigInt: BigInt = BigInt(this.hexString, 16)

    def toString(base: Base): String =
        base match
        case Base.Hex => this.hexString.toLowerCase.nn
        case Base.Dec => HexString.hexDecode(this)

    override def toString(): String = this.toString(Base.Hex)

    override def equals(x: Any): Boolean =
        if x.isInstanceOf[HexString] then this.toBigInt == x.asInstanceOf[HexString].toBigInt else false

//this.toString == x.toString

object HexString:   //  TODO: make `HeString` Chunk aware, both Chunk[Byte] and Chunk[Char]
    def apply(s: String): HexString =
        if s.isEmpty()
        then throw new IllegalArgumentException("Cannot create HexString from empty string")
        else new HexString(s)

    implicit val decoder: JsonDecoder[HexString] = JsonDecoder[String].map(HexString(_))
    implicit val encoder: JsonEncoder[HexString] = JsonEncoder[String].contramap(_.toString)

    def isHex(s: String): Boolean = s.matches("^[0-9a-fA-F\\s]+$")

    def bytesToHex(bytes: Array[Byte]): HexString =
        HexString(bytes.map("%02x".format(_)).mkString)

    def bigIntToHex(bigInt: BigInt): HexString =
        HexString(bigInt.toString(16))

    private def hexEncode(string: String): HexString =
        bytesToHex(string.getBytes(StandardCharsets.UTF_8).nn)

    private def hexDecode(hex: HexString): String =
        new String(hex.toByteArray, StandardCharsets.UTF_8).nn
