package is.clipperz.backend

import zio.ZIO
import zio.test.TestResult.any
import zio.test.{ ZIOSpecDefault, assertTrue, check }
import is.clipperz.backend.functions.Conversions.{ bytesToBigInt, bigIntToBytes }
import is.clipperz.backend.data.HexString.{ bytesToHex, bigIntToHex }
import is.clipperz.backend.services.PRNG
import zio.test.Gen
import zio.test.TestAspect
import is.clipperz.backend.functions.ByteArrays
import zio.test.TestResult
import is.clipperz.backend.services.OneTimeSecret
import zio.stream.ZStream
import zio.Chunk
import is.clipperz.backend.functions.fromStream
import zio.json.DeriveJsonDecoder
import zio.json.JsonEncoder
import zio.json.DeriveJsonEncoder
import zio.json.JsonDecoder

import com.github.nscala_time.time.Imports.*
import is.clipperz.backend.data.HexString

object ConversionsSpec extends ZIOSpecDefault:
  val samples = 10

  def spec = suite("Conversions")(
    test("bigint to byte array and back") {
      check(Gen.int) { i =>
        val bigInt = BigInt(i.toHexString, 16)
        assertTrue(bigInt == bytesToBigInt(bigIntToHex(bigInt).toByteArray))
      }
    } @@ TestAspect.samples(samples),
    test("bigint to byte array") {
      check(Gen.int) { i =>
        val bigInt = BigInt(i.toHexString, 16)
        equalsUpToEmptyByte(bigInt.toByteArray, bigIntToBytes(bigInt))
      }
    } @@ TestAspect.samples(samples),
    test("byte array to bigint") {
      for {
        prng <- ZIO.service[PRNG]
        res <- check(TestUtilities.getBytesGen(prng, 16)) { bytes =>
          equalsUpToEmptyByte(bytes, bytesToBigInt(bytes).toByteArray)
        }
      } yield res
    } @@ TestAspect.samples(samples),
    test("json to case class") {
      for {
        json <- ZIO.succeed(ZStream.fromChunks(Chunk.fromArray("""{"secret":"be031e72766940","creationDate":"2023-08-12T14:39:59.462Z","expirationDate":"2023-08-12T14:49:59.462Z"}""".getBytes().nn)))
        parsed <- fromStream[OneTimeSecret](json)
      } yield assertTrue(parsed == OneTimeSecret(HexString("be031e72766940"), DateTime.parse("2023-08-12T14:49:59.462Z"), Option.empty))
    }
  ).provideSomeLayer(PRNG.live)

  def equalsUpToEmptyByte(bytes1: Array[Byte], bytes2: Array[Byte]): TestResult =
    any(assertTrue((0.toByte +: bytes1) == bytes2), assertTrue(bytes1 == bytes2), assertTrue(bytes1 == (0.toByte +: bytes2)))
