package is.clipperz.backend.apis

// import java.nio.file.FileSystems
import zio.ZIO
import zio.stream.ZStream
import zio.test.Assertion.isTrue
import zio.test.{ ZIOSpecDefault, assertTrue, assertZIO, check }
import zio.test.TestAspect.timeout
import is.clipperz.backend.functions.SrpFunctions.{ baseConfiguration, SrpFunctionsV6a }
import is.clipperz.backend.data.HexString
import is.clipperz.backend.data.HexString.{ bigIntToHex, bytesToHex }
import is.clipperz.backend.functions.Conversions.{ bytesToBigInt, bigIntToBytes }
import is.clipperz.backend.functions.crypto.HashFunction
import is.clipperz.backend.functions.crypto.HashFunction.hashSHA1
import is.clipperz.backend.functions.crypto.KeyDerivationFunction.kdfSHA1
import is.clipperz.backend.services.UserArchive
import is.clipperz.backend.services.PRNG
import is.clipperz.backend.services.SessionManager
import is.clipperz.backend.services.SrpManager
import is.clipperz.backend.services.UserCard
import is.clipperz.backend.services.SRPStep1Data
import is.clipperz.backend.services.SRPStep2Data
import is.clipperz.backend.TestUtilities
import zio.test.TestAspect
import zio.test.Gen
import zio.test.Spec
import is.clipperz.backend.data.srp.RFCTestVector
import is.clipperz.backend.data.srp.SRPGroup.apply
import is.clipperz.backend.data.srp.SRPConfigV6a
import is.clipperz.backend.data.srp.SRPGroup
import is.clipperz.backend.data.srp.SRPTestVector

object SrpFunctionsSpec extends ZIOSpecDefault:
  val samples = 10

  val testVectors = List(RFCTestVector)

  def makeTestsFromVectors(testVector: SRPTestVector) =
    val srpFunctions = new SrpFunctionsV6a(SRPConfigV6a(SRPGroup(testVector.nn, testVector.g), testVector.k, hashSHA1, kdfSHA1))
    test(s"compute aa - ${testVector}") {
      assertTrue(srpFunctions.computeA(testVector.a) == testVector.aa)
    } +
      test(s"compute bb - ${testVector}") {
        assertTrue(srpFunctions.computeB(testVector.b, testVector.v) == testVector.bb)
      } +
      test(s"compute server secret - ${testVector}") {
        assertTrue(srpFunctions.computeSecretServer(testVector.aa, testVector.b, testVector.v, testVector.u) == testVector.secret)
      } +
      test(s"compute client secret - ${testVector}") {
        assertTrue(srpFunctions.computeSecretClient(testVector.bb, testVector.x, testVector.a, testVector.u) == testVector.secret)
      } +
      test(s"compute v - ${testVector}"):
        assertTrue(srpFunctions.computeV(testVector.x) == testVector.v)
      /* +
    test(s"compute u - ${testVector}") {
      srpFunctions
        .computeU(testVector.aa.toByteArray, testVector.bb.toByteArray)
        .map(bytes => bytesToHex(bytes).toString())
        .map(hex => assertTrue(hex == testVector.u.toString(16)))
    } +
    test(s"compute k - ${testVector}") {
      srpFunctions
        .computeK(testVector.s)
        .map(bytes => assertTrue(bytesToBigInt(bytes) == testVector.k))
    } */

  def spec = suite("SrpFunctions")(
    testVectors.map(makeTestsFromVectors(_)).reduce(_ + _) +
    test("test core SRP base configuration parameters") {
      val nn =
        "167609434410335061345139523764350090260135525329813904557420930309800865859473551531551523800013916573891864789934747039010546328480848979516637673776605610374669426214776197828492691384519453218253702788022233205683635831626913357154941914129985489522629902540768368409482248290641036967659389658897350067939"
      assertTrue(baseConfiguration.group.nn.toString() == nn)
      val g = BigInt(2)
      assertTrue(baseConfiguration.group.g == BigInt(2))
      val k = BigInt("669884594844073113358786362162819048475760728175", 10)
      assertTrue(baseConfiguration.k == k)
    } +
    test("test secret generation by client and server") {
      for {
        prng <- ZIO.service[PRNG]
        res <- check(
          TestUtilities.getBytesGen(prng, 32),
          TestUtilities.getBytesGen(prng, 32),
          TestUtilities.getBytesGen(prng, 64),
          TestUtilities.getBytesGen(prng, 64),
        ) { (pBytes, sBytes, a, b) =>
          val srpFunctions = new SrpFunctionsV6a()

          val pHex = HexString.bytesToHex(pBytes)
          val sHex = HexString.bytesToHex(sBytes)

          for {
            aa <- ZIO.succeed(srpFunctions.computeA(bytesToBigInt(a)))
            x <- srpFunctions.configuration.keyDerivationFunction(sBytes, pBytes).map(bytes => bytesToBigInt(bytes))
            v <- ZIO.succeed(srpFunctions.computeV(x))
            bb <- ZIO.succeed(srpFunctions.computeB(bytesToBigInt(b), v))
            u <- srpFunctions.computeU(bigIntToBytes(aa), bigIntToBytes(bb))
            secretClient <- ZIO.succeed(srpFunctions.computeSecretClient(bb, x, bytesToBigInt(a), bytesToBigInt(u)))
            secretServer <- ZIO.succeed(srpFunctions.computeSecretServer(aa, bytesToBigInt(b), v, bytesToBigInt(u)))
          } yield assertTrue(secretClient == secretServer)
        }
      } yield res
    } @@ TestAspect.samples(samples) +
    test("hash of byte array") {
      val hex = HexString(
        "a27054ba67c93082d84d7c525f26ffa12d7de961419bbd5d09c4e09c01bb50280f1badda26f69993aea56d1689aebfb42947a63ccb09932de0d07cb457f9691853f765a16baf77ede507cf358b35de4cfb417b246745d875e70b1d463b200789e5edfd7ab6b3c888a296e690d52e7e235152a25ee6fa8354d1bc0138bf95cea"
      )
      val ab = hex.toByteArray
      val result = HexString("3612c14a51e43e9dd998731caafbe956cda617279d62cb5d36fd249347e08dad")
      for {
        hash <- HashFunction.hashSHA256(ZStream.fromIterable(ab)).map(b => bytesToHex(b))
      } yield assertTrue(hash == result)
    }
  ).provideLayerShared(PRNG.live)
