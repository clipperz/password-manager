package is.clipperz.backend

import zio.ZIO
import zio.test.{ ZIOSpecDefault, assertTrue, check }
import is.clipperz.backend.functions.Conversions.{ bytesToBigInt }
import is.clipperz.backend.data.HexString.{ bytesToHex, bigIntToHex }
import is.clipperz.backend.services.PRNG
import zio.test.Gen
import zio.test.TestAspect

object SrpFunctionsConversionsSuite extends ZIOSpecDefault:
  val samples = 10

  def spec = suite("conversions")(
    test("bigint to byte array") {
      check(Gen.int) { i =>
        val bigInt = BigInt(i)
        assertTrue(bigInt == bytesToBigInt(bigIntToHex(bigInt).toByteArray))
      }
    } @@ TestAspect.samples(samples)
  )
