package is.clipperz.backend.apis

import java.nio.file.FileSystems
import zio.ZIO
import zio.stream.ZStream
import zio.test.{ ZIOSpecDefault, assertTrue }
import zio.test.TestAspect.timeout
import is.clipperz.backend.functions.SrpFunctions.{ baseConfiguration, SrpFunctionsV6a }
import is.clipperz.backend.data.HexString
import is.clipperz.backend.data.HexString.{ bigIntToHex, bytesToHex }
import is.clipperz.backend.functions.Conversions.{ bytesToBigInt, bigIntToBytes }
import is.clipperz.backend.functions.crypto.HashFunction
import is.clipperz.backend.services.UserArchive
import is.clipperz.backend.services.PRNG
import is.clipperz.backend.services.SessionManager
import is.clipperz.backend.services.SrpManager
import is.clipperz.backend.services.UserCard
import is.clipperz.backend.services.SRPStep1Data
import is.clipperz.backend.services.SRPStep2Data

object SrpManangerSuite extends ZIOSpecDefault:
  val blobBasePath = FileSystems.getDefault().nn.getPath("target", "tests", "archive", "blobs").nn
  val userBasePath = FileSystems.getDefault().nn.getPath("target", "tests", "archive", "users").nn

  val archive = UserArchive.fs(userBasePath, 2)

  val environment =
    archive ++
    PRNG.live ++
    SessionManager.live ++
    ((archive ++ PRNG.live) >>> SrpManager.v6a())

  def spec = suite("SrpManager")(
    test("test core SRP base configuration parameters") {
      val nn = "167609434410335061345139523764350090260135525329813904557420930309800865859473551531551523800013916573891864789934747039010546328480848979516637673776605610374669426214776197828492691384519453218253702788022233205683635831626913357154941914129985489522629902540768368409482248290641036967659389658897350067939"
      assertTrue(baseConfiguration.group.nn.toString() == nn)
      val g = BigInt(2)
      assertTrue(baseConfiguration.group.g == BigInt(2))
      val k = BigInt("669884594844073113358786362162819048475760728175", 10)
      assertTrue(baseConfiguration.k == k)
    } +
    test("compute aa") {
      val srpFunctions = new SrpFunctionsV6a()
      val a = HexString("60975527 035CF2AD 1989806F 0407210B C81EDC04 E2762A56 AFD529DD DA2D4393".filterNot(_.isWhitespace)).toBigInt
      val aa = BigInt("""
        61D5E490 F6F1B795 47B0704C 436F523D D0E560F0 C64115BB 72557EC4
        4352E890 3211C046 92272D8B 2D1A5358 A2CF1B6E 0BFCF99F 921530EC
        8E393561 79EAE45E 42BA92AE ACED8251 71E1E8B9 AF6D9C03 E1327F44
        BE087EF0 6530E69F 66615261 EEF54073 CA11CF58 58F0EDFD FE15EFEA
        B349EF5D 76988A36 72FAC47B 0769447B
      """.filterNot(_.isWhitespace), 16)

      assertTrue(srpFunctions.computeA(a) == aa)
    } +
    test("compute bb") {  
      val bb = BigInt("""
        BD0C6151 2C692C0C B6D041FA 01BB152D 4916A1E7 7AF46AE1 05393011
        BAF38964 DC46A067 0DD125B9 5A981652 236F99D9 B681CBF8 7837EC99
        6C6DA044 53728610 D0C6DDB5 8B318885 D7D82C7F 8DEB75CE 7BD4FBAA
        37089E6F 9C6059F3 88838E7A 00030B33 1EB76840 910440B1 B27AAEAE
        EB4012B7 D7665238 A8E3FB00 4B117B58  
      """.filterNot(_.isWhitespace), 16)

      for {
        srpFunctions <- ZIO.succeed(new SrpFunctionsV6a())
        b <- ZIO.succeed(HexString("E487CB59 D31AC550 471E81F0 0F6928E0 1DDA08E9 74A004F4 9E61F5D1 05284D20".filterNot(_.isWhitespace)).toBigInt)
        v <- ZIO.succeed(BigInt("""
          7E273DE8 696FFC4F 4E337D05 B4B375BE B0DDE156 9E8FA00A 9886D812
          9BADA1F1 822223CA 1A605B53 0E379BA4 729FDC59 F105B478 7E5186F5
          C671085A 1447B52A 48CF1970 B4FB6F84 00BBF4CE BFBB1681 52E08AB5
          EA53D15C 1AFF87B2 B9DA6E04 E058AD51 CC72BFC9 033B564E 26480D78
          E955A5E2 9E7AB245 DB2BE315 E2099AFB
        """.filterNot(_.isWhitespace), 16))
        computedB <- ZIO.succeed(srpFunctions.computeB(b, v))
      } yield assertTrue(computedB == bb)

    } +
    test("compute server secret") {
      val srpFunctions = new SrpFunctionsV6a()

      val secret = BigInt("""B0DC82BA BCF30674 AE450C02 87745E79 90A3381F 63B387AA F271A10D
        233861E3 59B48220 F7C4693C 9AE12B0A 6F67809F 0876E2D0 13800D6C
        41BB59B6 D5979B5C 00A172B4 A2A5903A 0BDCAF8A 709585EB 2AFAFA8F
        3499B200 210DCC1F 10EB3394 3CD67FC8 8A2F39A4 BE5BEC4E C0A3212D
        C346D7E4 74B29EDE 8A469FFE CA686E5A
      """.filterNot(_.isWhitespace), 16)

      val aa = BigInt("""61D5E490 F6F1B795 47B0704C 436F523D D0E560F0 C64115BB 72557EC4
        4352E890 3211C046 92272D8B 2D1A5358 A2CF1B6E 0BFCF99F 921530EC
        8E393561 79EAE45E 42BA92AE ACED8251 71E1E8B9 AF6D9C03 E1327F44
        BE087EF0 6530E69F 66615261 EEF54073 CA11CF58 58F0EDFD FE15EFEA
        B349EF5D 76988A36 72FAC47B 0769447B
      """.filterNot(_.isWhitespace), 16)

      val v = BigInt("""7E273DE8 696FFC4F 4E337D05 B4B375BE B0DDE156 9E8FA00A 9886D812
        9BADA1F1 822223CA 1A605B53 0E379BA4 729FDC59 F105B478 7E5186F5
        C671085A 1447B52A 48CF1970 B4FB6F84 00BBF4CE BFBB1681 52E08AB5
        EA53D15C 1AFF87B2 B9DA6E04 E058AD51 CC72BFC9 033B564E 26480D78
        E955A5E2 9E7AB245 DB2BE315 E2099AFB
      """.filterNot(_.isWhitespace), 16)

      val b = HexString("E487CB59 D31AC550 471E81F0 0F6928E0 1DDA08E9 74A004F4 9E61F5D1 05284D20".filterNot(_.isWhitespace)).toBigInt
      val u = HexString("CE38B959 3487DA98 554ED47D 70A7AE5F 462EF019".filterNot(_.isWhitespace)).toBigInt

      assertTrue(srpFunctions.computeSecretServer(aa, b, v, u) == secret)
    } +
    test ("compute client secret") {
      val srpFunctions = new SrpFunctionsV6a()

      val secret = BigInt("""B0DC82BA BCF30674 AE450C02 87745E79 90A3381F 63B387AA F271A10D
        233861E3 59B48220 F7C4693C 9AE12B0A 6F67809F 0876E2D0 13800D6C
        41BB59B6 D5979B5C 00A172B4 A2A5903A 0BDCAF8A 709585EB 2AFAFA8F
        3499B200 210DCC1F 10EB3394 3CD67FC8 8A2F39A4 BE5BEC4E C0A3212D
        C346D7E4 74B29EDE 8A469FFE CA686E5A
      """.filterNot(_.isWhitespace), 16)

      val a = HexString("60975527 035CF2AD 1989806F 0407210B C81EDC04 E2762A56 AFD529DD DA2D4393".filterNot(_.isWhitespace)).toBigInt

      val bb = BigInt("""BD0C6151 2C692C0C B6D041FA 01BB152D 4916A1E7 7AF46AE1 05393011
        BAF38964 DC46A067 0DD125B9 5A981652 236F99D9 B681CBF8 7837EC99
        6C6DA044 53728610 D0C6DDB5 8B318885 D7D82C7F 8DEB75CE 7BD4FBAA
        37089E6F 9C6059F3 88838E7A 00030B33 1EB76840 910440B1 B27AAEAE
        EB4012B7 D7665238 A8E3FB00 4B117B58""".filterNot(_.isWhitespace), 16)

      val u = HexString("CE38B959 3487DA98 554ED47D 70A7AE5F 462EF019".filterNot(_.isWhitespace)).toBigInt
      val x = HexString("94B7555A ABE9127C C58CCF49 93DB6CF8 4D16C124".filterNot(_.isWhitespace)).toBigInt

      assertTrue(srpFunctions.computeSecretClient(bb, x, a, u) == secret)
    } +
    test("test secret generation by client and server") {
      val srpFunctions = new SrpFunctionsV6a()

      val pHex = HexString("0123456789012345678901234567890123456789012345678901234567890123") //TODO: generate from library
      val sHex = HexString("0123456789012345678901234567890123456789012345678901234567890123") //TODO: generate from library

      val sBytes = sHex.toByteArray
      val pBytes = pHex.toByteArray

      for {
        prng <- ZIO.service[PRNG]
        a <- prng.nextBytes(64)
        aa <- ZIO.succeed(srpFunctions.computeA(bytesToBigInt(a)))
        b <- prng.nextBytes(64)
        x <- srpFunctions.configuration.keyDerivationFunction(sBytes, pBytes)
              .map(bytes => bytesToBigInt(bytes))
        v <- ZIO.succeed(srpFunctions.computeV(x))
        bb <- ZIO.succeed(srpFunctions.computeB(bytesToBigInt(b), v))
        u <- srpFunctions.computeU(bigIntToBytes(aa), bigIntToBytes(bb))
        secretClient <- ZIO.succeed(srpFunctions.computeSecretClient(bb, x, bytesToBigInt(a), bytesToBigInt(u)))
        secretServer <- ZIO.succeed(srpFunctions.computeSecretServer(aa, bytesToBigInt(b), v, bytesToBigInt(u)))
      } yield assertTrue(secretClient == secretServer)
    } +
    test("hash of byte array") {
      val hex = HexString("a27054ba67c93082d84d7c525f26ffa12d7de961419bbd5d09c4e09c01bb50280f1badda26f69993aea56d1689aebfb42947a63ccb09932de0d07cb457f9691853f765a16baf77ede507cf358b35de4cfb417b246745d875e70b1d463b200789e5edfd7ab6b3c888a296e690d52e7e235152a25ee6fa8354d1bc0138bf95cea")
      val ab = hex.toByteArray
      val result = HexString("3612c14a51e43e9dd998731caafbe956cda617279d62cb5d36fd249347e08dad")
      for {
        hash <- HashFunction.hashSHA256(ZStream.fromIterable(ab)).map(b => bytesToHex(b))  
      } yield assertTrue (hash == result)
    } +
    test("SRP full round-trip") {
      val srpFunctions = new SrpFunctionsV6a()
      val config = srpFunctions.configuration
      
      val cHex = HexString("0123456789012345678901234567890123456789012345678901234567890123") //TODO: generate from library
      val pHex = HexString("0123456789012345678901234567890123456789012345678901234567890123") //TODO: generate from library
      val sHex = HexString("0123456789012345678901234567890123456789012345678901234567890123") //TODO: generate from library

      val sBytes = sHex.toByteArray

      val pBytes = pHex.toByteArray

      val c: Array[Byte] = cHex.toByteArray
      val s: Array[Byte] = sHex.toByteArray

      val nn = srpFunctions.configuration.group.nn

      for {
        x <- config.keyDerivationFunction(sBytes, pBytes)
              .map(bytes => bytesToBigInt(bytes))
        v <- ZIO.succeed(srpFunctions.computeV(x))
        card <- ZIO.succeed(UserCard(
          c = cHex,
          s = sHex,
          v = bigIntToHex(v),
          srpVersion = "srpVersion_testFullTrip",
          masterKeyEncodingVersion = "masterKeyEncodingVersion_testFullTrip",
          masterKeyContent = HexString("masterKeyContent_testFullTrip"),
        ))
        
        //-------------------
        
        userAchive <- ZIO.service[UserArchive]
        username <- userAchive.saveUser(card, true)
        
        //-------------------

        session <- ZIO.service[SessionManager]
        sessionContext <- session.getSession("test")
        srp <- ZIO.service[SrpManager]
        prng <- ZIO.service[PRNG]
        a <- prng.nextBytes(64)
        aa <- ZIO.succeed(srpFunctions.computeA(bytesToBigInt(a)))
        (step1Response, context) <- srp.srpStep1(SRPStep1Data(cHex, bigIntToHex(aa)), sessionContext)
        u <- srpFunctions.computeU(bigIntToBytes(aa), step1Response.bb.toByteArray)
        config <- ZIO.succeed(srpFunctions.configuration)
        x <- config.keyDerivationFunction(step1Response.s.toByteArray, pHex.toByteArray).map(bytes => bytesToBigInt(bytes))
        clientSecret <- ZIO.succeed(srpFunctions.computeSecretClient(step1Response.bb.toBigInt, x, bytesToBigInt(a), bytesToBigInt(u)))
        kk <- srpFunctions.configuration.hash(ZStream.fromIterable(bigIntToBytes(clientSecret)))
        m1 <- srpFunctions.computeM1(c, s, bigIntToBytes(aa), step1Response.bb.toByteArray, kk)
        (step2Response, context) <- srp.srpStep2(SRPStep2Data(bytesToHex(m1)), context)
        m2 <- srpFunctions.computeM2(bigIntToBytes(aa), m1, kk)
      } yield assertTrue(bytesToBigInt(m2) == step2Response.m2.toBigInt)
    }
  ).provideCustomLayerShared(environment)
