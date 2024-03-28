package is.clipperz.backend.services

import java.io.File
// import java.nio.charset.StandardCharsets
// import java.nio.file.{ Files, Paths, FileSystems }
import java.security.MessageDigest
import scala.language.postfixOps
import zio.{ Chunk, ZIO, Task }
import zio.stream.{ ZStream, ZSink }
import zio.test.Assertion.{ nothing, isTrue, isFalse, throws, throwsA, fails, isSubtype, anything }
import zio.test.{ ZIOSpecDefault, check, assertTrue, assert, assertCompletes, assertNever, assertZIO, TestAspect }
import zio.json.EncoderOps
import is.clipperz.backend.Main
// import java.nio.file.Path
import _root_.is.clipperz.backend.Exceptions.*
import is.clipperz.backend.functions.crypto.HashFunction
import zio.Clock
import zio.Clock.ClockLive
import zio.test.TestClock
import zio.Duration
import zio.test.Gen
import is.clipperz.backend.functions.ByteArrays
import is.clipperz.backend.data.HexString
import zio.test.Sized
import is.clipperz.backend.TestUtilities

object TollManagerSpec extends ZIOSpecDefault:
  val samples = 10
  val layers = PRNG.live ++ (PRNG.live >>> TollManager.live)

  val hexString = HexString("""EEAF0AB9ADB38DD69C33F80AFA8FC5E86072618775FF3C0B9EA2314C
                               9C256576D674DF7496EA81D3383B4813D692C6E0E0D5D8E250B98BE4
                               8E495C1D6089DAD15DC7D7B46154D6B6CE8EF4AD69B15D4982559B29
                               7BCF1885C529F566660E57EC68EDBC3C05726CC02FD4CBF4976EAA9A
                               FD5138FE8376435B9FC61D2FC0EB06E3""")
  val hexStringHash = ByteArrays
    .hashOfArrays(HashFunction.hashSHA256, hexString.toByteArray)
    .map(HexString.bytesToHex)

  def spec = suite("TollManager")(
    test("getToll - correct toll cost") { // property test
      for {
        manager <- ZIO.service[TollManager]
        res <- check(Gen.int) { i =>
          if i >= 0 then assertZIO(manager.getToll(i).map(_.cost == i))(isTrue)
          else assertZIO(manager.getToll(i).map(_.cost == i).exit)(fails(isSubtype[IllegalArgumentException](anything)))
        }
      } yield res
    } @@ TestAspect.samples(samples) +
      test("verifyToll - success") {
        for {
          manager <- ZIO.service[TollManager]
          prng <- ZIO.service[PRNG]
          res <- check(TestUtilities.getBytesGen(prng, tollByteSize)) { bytes =>
            for {
              hash <- ByteArrays
                .hashOfArrays(HashFunction.hashSHA256, bytes)
                .map(HexString.bytesToHex)
              res <- assertZIO(manager.verifyToll(TollChallenge(hash, 15), HexString.bytesToHex(bytes)))(isTrue)
            } yield res
          }
        } yield res
      } @@ TestAspect.samples(samples) +
      test("verifyToll - fail") { // TODO: improve
        for {
          manager <- ZIO.service[TollManager]
          prng <- ZIO.service[PRNG]
          res <- check(TestUtilities.getBytesGen(prng, tollByteSize)) { bytes =>
            for {
              hash <- ByteArrays
                .hashOfArrays(HashFunction.hashSHA256, bytes)
                .map(HexString.bytesToHex)
              res <- assertZIO(manager.verifyToll(TollChallenge(hash, 15), HexString("0000000000000")))(isFalse)
            } yield res
          }
        } yield res
      } @@ TestAspect.samples(samples) +
      test("verifyToll - fail - invalid challenge cost") { // TODO: improve
        for {
          manager <- ZIO.service[TollManager]
          hash <- hexStringHash
          res <- check(Gen.int) { i =>
            if i >= 0 then assertZIO(manager.verifyToll(TollChallenge(hash, i), hexString))(isTrue)
            else
              assertZIO(manager.verifyToll(TollChallenge(hash, i), hexString).exit)(
                fails(isSubtype[IllegalArgumentException](anything))
              )
          }
        } yield res
      } @@ TestAspect.samples(samples) +
      test("computeReceipt") {
        for {
          manager <- ZIO.service[TollManager]
          prng <- ZIO.service[PRNG]
          res <- check(TestUtilities.getBytesGen(prng, tollByteSize)) { bytes =>
            for {
              challenge <- ByteArrays
                .hashOfArrays(HashFunction.hashSHA256, bytes)
                .map(HexString.bytesToHex)
                .map(TollChallenge(_, 2))
              receipt <- TollManager.computeReceipt(prng, manager)(challenge)
              res <- assertZIO(manager.verifyToll(challenge, HexString.bytesToHex(bytes)))(isTrue)
            } yield res
          }
        } yield res
      } @@ TestAspect.samples(samples)
  ).provideSomeLayerShared(layers)
