package is.clipperz.backend

import zio.test.Assertion.{ isTrue, fails, isSubtype, anything }
import zio.test.{ ZIOSpecDefault, assertTrue, assertCompletes, assertZIO, check }
import is.clipperz.backend.data.HexString
import is.clipperz.backend.data.Base
import is.clipperz.backend.TestUtilities
import zio.ZIO
import is.clipperz.backend.services.TollManager
import is.clipperz.backend.services.PRNG
import zio.test.TestAspect
import zio.test.Gen
// import java.nio.charset.StandardCharsets

object HexStringSpec extends ZIOSpecDefault:
  val samples = 10

  def spec = suite("HexString")(
    test("isHex - success and fail") {
      val hexString = """EEAF0AB9ADB38DD69C33F80AFA8FC5E86072618775FF3C0B9EA2314C
                         9C256576D674DF7496EA81D3383B4813D692C6E0E0D5D8E250B98BE4
                         8E495C1D6089DAD15DC7D7B46154D6B6CE8EF4AD69B15D4982559B29
                         7BCF1885C529F566660E57EC68EDBC3C05726CC02FD4CBF4976EAA9A
                         FD5138FE8376435B9FC61D2FC0EB06E3"""
      val string = "tschüs"
      assertTrue(HexString.isHex(hexString), !HexString.isHex(string))
    },
    test("create HexString") {
      check(Gen.stringN(samples)(Gen.hexCharUpper)) { hexString =>
        val lowercaseHex = hexString.toLowerCase().nn
        val hexStringFromConstructor = (HexString(hexString)).toString()
        assertTrue(lowercaseHex == hexStringFromConstructor)
      }
    } @@ TestAspect.samples(samples),
    test("compare HexString") {
      check(Gen.stringN(9)(Gen.hexCharUpper)) { hexString =>
        val hexStringOdd = HexString(hexString)
        val hexStringEven = HexString(s"0${hexString}")
        assertTrue(hexStringEven == hexStringOdd)
      }
    } @@ TestAspect.samples(samples),
    test("check hexEncode correctness") {
      val fromString = HexString("tschüs")
      val fromHex = HexString("74736368c3bc73")
      assertTrue(fromHex == fromString)
    },
    test("check hexDecode correctness") {
      val str = "tschüs"
      val fromHex = HexString("74736368c3bc73").toString(Base.Dec)
      assertTrue(str == fromHex)
    },
    test("hex to bytes and back") {
      check(Gen.stringN(9)(Gen.hexCharLower)) { hexString =>
        val hex = HexString(hexString)
        assertTrue(HexString.bytesToHex(hex.toByteArray) == hex)
      }
    } @@ TestAspect.samples(samples),
    test("hex to bigint and back") {
      check(Gen.stringN(9)(Gen.hexCharLower)) { hexString =>
        val hex = HexString(hexString)
        assertTrue(HexString.bigIntToHex(hex.toBigInt) == hex)
      }
    } @@ TestAspect.samples(samples),
    test("bytes to hex and back") {
      for {
        prng <- ZIO.service[PRNG]
        res <- check(TestUtilities.getBytesGen(prng, 16)) { bytes =>
          assertTrue(HexString.bytesToHex(bytes).toByteArray == bytes)
        }
      } yield res
    } @@ TestAspect.samples(samples),
    test("bigint to hex and back") {
      check(Gen.int) { i =>
        // TODO: cannot directly convert from int to bigint because negative numbers are not correctly managed when covnerting from bigint to hexstring
        val bi = BigInt(i.toHexString, 16)
        assertTrue(HexString.bigIntToHex(bi).toBigInt == bi)
      }
    } @@ TestAspect.samples(samples),
    test("isHex - success") {
      check(Gen.stringN(9)(Gen.hexChar))(s => assertTrue(HexString.isHex(s)))
    } @@ TestAspect.samples(samples),
    test("isHex - fail") {
      check(Gen.alphaNumericString) { s =>
        if s.matches("^[0-9a-fA-F\\s]+$") then assertCompletes
        else assertTrue(!HexString.isHex(s))
      }
    } @@ TestAspect.samples(samples),
    test("hex from empty string - fail") {
      assertZIO(ZIO.attempt(HexString("")).exit)(fails(isSubtype[IllegalArgumentException](anything)))
    },
    test("hex encode and decode roundtrip") {
      check(Gen.alphaNumericStringBounded(1, 50)) { s =>
        if HexString.isHex(s) then // checked in previous tests
          assertCompletes
        else assertTrue(HexString(s).toString(Base.Dec) == s)
      }
    } @@ TestAspect.samples(samples),
  ).provideSomeLayer(PRNG.live)
