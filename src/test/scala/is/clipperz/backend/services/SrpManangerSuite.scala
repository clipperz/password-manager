package is.clipperz.backend.apis

// import java.nio.file.FileSystems
import zio.ZIO
import zio.nio.file.{ FileSystem }
import zio.stream.ZStream
import zio.test.{ ZIOSpecDefault, assertTrue, assertNever, assertZIO, check }
import zio.test.Assertion.{ fails, isSubtype, anything, isTrue }
import zio.test.TestAspect.timeout
import zio.test.TestResult.allSuccesses
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
import is.clipperz.backend.TestUtilities
import zio.test.TestAspect
import is.clipperz.backend.Exceptions.*
import zio.http.*
import is.clipperz.backend.services.RemoteUserCard
import is.clipperz.backend.services.SRPVersion
import is.clipperz.backend.services.MasterKeyEncodingVersion

object SrpManangerSpec extends ZIOSpecDefault:
  val samples = 10

  val blobBasePath = FileSystem.default.getPath("target", "tests", "archive", "blobs")
  val userBasePath = FileSystem.default.getPath("target", "tests", "archive", "users")

  val keyBlobArchiveFolderDepth = 16
  val archive = UserArchive.fs(userBasePath, keyBlobArchiveFolderDepth, false)

  val layers =
    archive ++
      PRNG.live ++
      (PRNG.live >>> SessionManager.live()) ++
      ((archive ++ PRNG.live) >>> SrpManager.v6a())

  val testRequestEmpty = Request(
    url = URL(Root),
    method = Method.GET,
    headers = Headers((SessionManager.sessionKeyHeaderName, "test")),
    body = Body.empty,
    version = Version.Http_1_1,
    remoteAddress = None
  )

  def spec = suite("SrpManager")(
    test("SRP step 1 - success") {
      for {
        prng <- ZIO.service[PRNG]
        res <- check(
          TestUtilities.getBytesGen(prng, 32),
          TestUtilities.getBytesGen(prng, 32),
          TestUtilities.getBytesGen(prng, 32),
          TestUtilities.getBytesGen(prng, 64),
        ) { (c, p, s, a) =>
          val srpFunctions = new SrpFunctionsV6a()
          val config = srpFunctions.configuration
          val cHex = HexString.bytesToHex(c)
          val pHex = HexString.bytesToHex(p)
          val sHex = HexString.bytesToHex(s)
          for {
            x <- config.keyDerivationFunction(s, p).map(bytes => bytesToBigInt(bytes))
            v <- ZIO.succeed(srpFunctions.computeV(x))
            card <- ZIO.succeed(
              RemoteUserCard(
                c = cHex,
                s = sHex,
                v = bigIntToHex(v),
                srpVersion = SRPVersion("srpVersion_testFullTrip"),
                // originMasterKey = None,
                // masterKeyEncodingVersion = "masterKeyEncodingVersion_testFullTrip",
                masterKey = (HexString("masterKeyContent_testFullTrip"), MasterKeyEncodingVersion("masterKeyEncodingVersion_testFullTrip"))
              )
            )

            userAchive <- ZIO.service[UserArchive]
            username <- userAchive.saveUser(card, true)

            session <- ZIO.service[SessionManager]
            sessionContext <- session.getSession(testRequestEmpty)

            srp <- ZIO.service[SrpManager]
            aa <- ZIO.succeed(srpFunctions.computeA(bytesToBigInt(a)))
            (step1Response, context) <- srp.srpStep1(SRPStep1Data(cHex, bigIntToHex(aa)), sessionContext)
            bb <- ZIO.succeed(bigIntToHex(srpFunctions.computeB(BigInt(context.content("b"), 16), v)))
          } yield allSuccesses(
            assertTrue(context.content("c") == cHex.toString()),
            assertTrue(context.content.isDefinedAt("b")),
            assertTrue(context.content("B") == bb.toString()),
            assertTrue(context.content("A") == bigIntToHex(aa).toString()),
            assertTrue(step1Response.s == sHex),
            assertTrue(step1Response.bb == bb),
          )
        }
      } yield res
    } @@ TestAspect.samples(samples),
    test("SRP step 1 - fail") {
      for {
        prng <- ZIO.service[PRNG]
        res <- check(
          TestUtilities.getBytesGen(prng, 32),
          TestUtilities.getBytesGen(prng, 32),
          TestUtilities.getBytesGen(prng, 32),
          TestUtilities.getBytesGen(prng, 64),
        ) { (c, p, s, a) =>
          val srpFunctions = new SrpFunctionsV6a()
          val cHex = HexString.bytesToHex(c)
          val pHex = HexString.bytesToHex(p)
          val sHex = HexString.bytesToHex(s)
          for {
            srp <- ZIO.service[SrpManager]
            session <- ZIO.service[SessionManager]
            sessionContext <- session.getSession(testRequestEmpty)
            aa <- ZIO.succeed(srpFunctions.computeA(bytesToBigInt(a)))
            res <- assertZIO(srp.srpStep1(SRPStep1Data(cHex, bigIntToHex(aa)), sessionContext).exit)(
              fails(isSubtype[ResourceNotFoundException](anything))
            )
          } yield res
        }
      } yield res
    } @@ TestAspect.samples(samples),
    test("SRP full round-trip") {
      for {
        prng <- ZIO.service[PRNG]
        res <- check(
          TestUtilities.getBytesGen(prng, 32),
          TestUtilities.getBytesGen(prng, 32),
          TestUtilities.getBytesGen(prng, 32),
          TestUtilities.getBytesGen(prng, 64),
        ) { (c, p, s, a) =>
          val srpFunctions = new SrpFunctionsV6a()
          val config = srpFunctions.configuration

          val cHex = HexString.bytesToHex(c)
          val pHex = HexString.bytesToHex(p)
          val sHex = HexString.bytesToHex(s)

          val nn = srpFunctions.configuration.group.nn

          for {
            x <- config.keyDerivationFunction(s, p).map(bytes => bytesToBigInt(bytes))
            v <- ZIO.succeed(srpFunctions.computeV(x))
            card <- ZIO.succeed(
              RemoteUserCard(
                c = cHex,
                s = sHex,
                v = bigIntToHex(v),
                srpVersion = SRPVersion("srpVersion_testFullTrip"),
                // originMasterKey = None,
                // masterKeyEncodingVersion = "masterKeyEncodingVersion_testFullTrip",
                masterKey = (HexString("masterKeyContent_testFullTrip"), MasterKeyEncodingVersion("masterKeyEncodingVersion_testFullTrip"))
              )
            )

            userAchive <- ZIO.service[UserArchive]
            username <- userAchive.saveUser(card, true)

            session <- ZIO.service[SessionManager]
            sessionContext <- session.getSession(testRequestEmpty)
            srp <- ZIO.service[SrpManager]
            aa <- ZIO.succeed(srpFunctions.computeA(bytesToBigInt(a)))
            (step1Response, context) <- srp.srpStep1(SRPStep1Data(cHex, bigIntToHex(aa)), sessionContext)
            u <- srpFunctions.computeU(bigIntToBytes(aa), step1Response.bb.toByteArray)
            config <- ZIO.succeed(srpFunctions.configuration)
            x <- config.keyDerivationFunction(step1Response.s.toByteArray, pHex.toByteArray).map(bytes => bytesToBigInt(bytes))
            clientSecret <- ZIO.succeed(
              srpFunctions.computeSecretClient(step1Response.bb.toBigInt, x, bytesToBigInt(a), bytesToBigInt(u))
            )
            kk <- srpFunctions.configuration.hash(ZStream.fromIterable(bigIntToBytes(clientSecret)))
            m1 <- srpFunctions.computeM1(c, s, bigIntToBytes(aa), step1Response.bb.toByteArray, kk)
            (step2Response, context) <- srp.srpStep2(SRPStep2Data(bytesToHex(m1)), context)
            m2 <- srpFunctions.computeM2(bigIntToBytes(aa), m1, kk)
          } yield assertTrue(bytesToBigInt(m2) == step2Response.m2.toBigInt)
        }
      } yield res
    } @@ TestAspect.samples(samples),
  ).provideSomeLayerShared(layers)
