package is.clipperz.backend

import zio.ZIO
import zio.test.{ ZIOSpecDefault, assertTrue }
import is.clipperz.backend.functions.Conversions.{ bytesToBigInt }
import is.clipperz.backend.data.HexString.{ bytesToHex, bigIntToHex }
import is.clipperz.backend.services.PRNG

object SrpFunctionsConversionsSuite extends ZIOSpecDefault:
  val environment =
    PRNG.live

  def spec = suite("conversions")(
    test("hex to byte array") {
      for {
        prng <- ZIO.service[PRNG]
        randomBytes <- prng.nextBytes(64)
      } yield assertTrue(randomBytes == bytesToHex(randomBytes).toByteArray)
    } + 
    test("bigint to byte array") {
      val bigInt = BigInt("200", 10)
      assertTrue(bigInt == bytesToBigInt(bigIntToHex(bigInt).toByteArray))
    }
  ).provideCustomLayerShared(environment)
