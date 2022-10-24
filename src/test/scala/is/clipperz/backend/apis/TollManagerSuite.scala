package is.clipperz.backend.services

import zio.ZIO
import zio.test.{ ZIOSpecDefault, assertTrue }
import is.clipperz.backend.data.HexString
import is.clipperz.backend.data.Base
import is.clipperz.backend.services.TollManager.*
import is.clipperz.backend.functions.ByteArrays
import is.clipperz.backend.functions.crypto.HashFunction

object TollManagerSuite extends ZIOSpecDefault:

  val environment =
    (PRNG.live >>> TollManager.live)

  def spec = suite("tollManager")(
    test("check receipt") {
      def hexString = HexString("""EEAF0AB9ADB38DD69C33F80AFA8FC5E86072618775FF3C0B9EA2314C
                    9C256576D674DF7496EA81D3383B4813D692C6E0E0D5D8E250B98BE4
                    8E495C1D6089DAD15DC7D7B46154D6B6CE8EF4AD69B15D4982559B29
                    7BCF1885C529F566660E57EC68EDBC3C05726CC02FD4CBF4976EAA9A
                    FD5138FE8376435B9FC61D2FC0EB06E3""")
      for {
        tollManager <- ZIO.service[TollManager]
        hash <- ByteArrays.hashOfArrays(HashFunction.hashSHA256, hexString.toByteArray)
                          .map(HexString.bytesToHex)
        result <- tollManager.verifyToll(TollChallenge(hash, 15), hexString)
      } yield assertTrue(result)
    }
  ).provideCustomLayerShared(environment)


