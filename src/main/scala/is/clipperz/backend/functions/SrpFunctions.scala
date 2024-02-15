package is.clipperz.backend.functions

import is.clipperz.backend.data.HexString
import is.clipperz.backend.data.srp.{ SRPGroup, SRPConfigV6a }
import is.clipperz.backend.functions.crypto.HashFunction
import is.clipperz.backend.functions.crypto.HashFunction.hashSHA256
import is.clipperz.backend.functions.crypto.KeyDerivationFunction.kdfSHA256
import is.clipperz.backend.functions.Conversions.{ bytesToBigInt, bigIntToBytes }
import is.clipperz.backend.functions.ByteArrays.{ hashOfArrays, bitxor }

import java.security.MessageDigest

import zio.{ ZIO, Task, Chunk }
import zio.stream.{ ZStream, ZSink }

trait SrpFunctions:
  def computeA(a: BigInt): BigInt
  def computeB(b: BigInt, v: BigInt): BigInt
  def computeSecretServer(
      aa: BigInt,
      b: BigInt,
      v: BigInt,
      u: BigInt,
    ): BigInt
  def computeSecretClient(
      aa: BigInt,
      b: BigInt,
      v: BigInt,
      u: BigInt,
    ): BigInt
  def computeU(aa: Array[Byte], bb: Array[Byte]): Task[Array[Byte]]
  def computeV(x: BigInt): BigInt
  def computeM1(
      c: Array[Byte],
      s: Array[Byte],
      aa: Array[Byte],
      bb: Array[Byte],
      kk: Array[Byte],
    ): Task[Array[Byte]]
  def computeM2(
      aa: Array[Byte],
      m1: Array[Byte],
      kk: Array[Byte],
    ): Task[Array[Byte]]
  def computeK(ss: BigInt): Task[Array[Byte]]

object SrpFunctions:
  val baseConfiguration: SRPConfigV6a =
    SRPConfigV6a(SRPGroup.group1024, HexString("7556AA045AEF2CDD07ABAF0F665C3E818913186F").toBigInt, hashSHA256, kdfSHA256)

  case class SrpFunctionsV6a(configuration: SRPConfigV6a = baseConfiguration) extends SrpFunctions:
    override def computeA(a: BigInt): BigInt =
      val nn = this.configuration.group.nn
      val g = this.configuration.group.g
      g.modPow(a, nn)

    override def computeB(b: BigInt, v: BigInt): BigInt =
      val nn = this.configuration.group.nn
      val g = this.configuration.group.g
      val k = this.configuration.k
      ((k * v).mod(nn) + g.modPow(b, nn)).mod(nn)

    override def computeSecretServer(
        aa: BigInt,
        b: BigInt,
        v: BigInt,
        u: BigInt,
      ): BigInt =
      val nn = this.configuration.group.nn
      (aa * (v.modPow(u, nn))).mod(nn).modPow(b, nn)

    override def computeSecretClient(
        bb: BigInt,
        x: BigInt,
        a: BigInt,
        u: BigInt,
      ): BigInt =
      val nn = this.configuration.group.nn
      val g = this.configuration.group.g
      val k = this.configuration.k
      val base: BigInt = (bb - (k * g.modPow(x, nn)).mod(nn)).mod(nn)
      val exp: BigInt = a + (u * x)
      base.modPow(exp, nn)

    override def computeU(aa: Array[Byte], bb: Array[Byte]): Task[Array[Byte]] =
      hashOfArrays(baseConfiguration.hash, aa, bb)

    override def computeV(x: BigInt): BigInt =
      val nn = this.configuration.group.nn
      val g = this.configuration.group.g
      g.modPow(x, nn)

    override def computeM1(
        c: Array[Byte],
        s: Array[Byte],
        aa: Array[Byte],
        bb: Array[Byte],
        kk: Array[Byte],
      ): Task[Array[Byte]] =
      val nn = this.configuration.group.nn
      val g = this.configuration.group.g
      for {
        hashN <- baseConfiguration.hash(ZStream.fromIterable(bigIntToBytes(nn)))
        hashg <- baseConfiguration.hash(ZStream.fromIterable(bigIntToBytes(g)))
        gXorN <- ZIO.succeed(bitxor(hashN, hashg))
        hashc <- baseConfiguration.hash(ZStream.fromIterable(c))
        m1 <- hashOfArrays(baseConfiguration.hash, gXorN, hashc, s, aa, bb, kk)
      } yield m1

    override def computeM2(
        aa: Array[Byte],
        m1: Array[Byte],
        kk: Array[Byte],
      ): Task[Array[Byte]] =
      hashOfArrays(baseConfiguration.hash, aa, m1, kk)

    override def computeK(ss: BigInt): Task[Array[Byte]] =
      baseConfiguration.hash(ZStream.fromIterable(bigIntToBytes(ss)))
