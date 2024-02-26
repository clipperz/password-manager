package is.clipperz.backend.apis

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Paths, FileSystems }
import java.security.MessageDigest

import scala.language.postfixOps

import zio.{ Chunk, ZIO, durationInt }
import zio.json.EncoderOps
import zio.http.{ Body, Boundary, Form, FormField, Headers, MediaType, Method, Request, Root, URL, Version }
// import zio.http.*
import zio.stream.{ ZStream, ZSink }
import zio.test.{ ZIOSpecDefault, assertTrue, assert, TestAspect }
import zio.test.Assertion.{ nothing }
import zio.test.TestResult.{ allSuccesses }

import is.clipperz.backend.Main
import is.clipperz.backend.data.{ Base, HexString }
import is.clipperz.backend.data.HexString.bytesToHex
import is.clipperz.backend.functions.{ FileSystem, customErrorHandler, fromStream }
import is.clipperz.backend.functions.crypto.HashFunction
import is.clipperz.backend.services.{ BlobArchive, OneTimeShareArchive, PRNG, SessionManager, SrpManager, UserArchive, TollManager }


object BlobSpec extends ZIOSpecDefault:
    val app =  (   blobsApi        
               ).handleErrorCauseZIO(customErrorHandler)
                .toHttpApp
    val blobBasePath = FileSystems.getDefault().nn.getPath("target", "tests", "archive", "blobs").nn
    val userBasePath = FileSystems.getDefault().nn.getPath("target", "tests", "archive", "users").nn
    val oneTimeShareBasePath = FileSystems.getDefault().nn.getPath("target", "tests", "archive", "one_time_share").nn

    val boundary = "--TestBoundary"

    val environment =
        PRNG.live ++
        (PRNG.live >>> SessionManager.live()) ++
        UserArchive.fs(userBasePath, 2, false) ++
        BlobArchive.fs(blobBasePath, 2, false) ++
        OneTimeShareArchive.fs(oneTimeShareBasePath, 2, false) ++
        ((UserArchive.fs(userBasePath, 2, false) ++ PRNG.live) >>> SrpManager.v6a()) ++
        (PRNG.live >>> TollManager.live)

    val largeBlobData: Array[Byte] =
        Files
            .readAllBytes(Paths.get("src/test/resources/blobs/4073041693a9a66983e6ffb75b521310d30e6db60afc0f97d440cb816bce7c63.blob"))
            .nn
    val largeBlobHash = HexString("4073041693a9a66983e6ffb75b521310d30e6db60afc0f97d440cb816bce7c63")

    val validBlobHash = HexString("f9032dd04636e22b80db4c87513952154b05df9bc15c6951a5a73d810e1c5cae")
    val validBlobData = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nam ante massa, congue a sapien vel, efficitur facilisis eros. Mauris varius leo ut dolor malesuada, a pretium est scelerisque. Integer ut.".getBytes().nn
    val identifier    = HexString("affa")

    def post (hash: HexString, identifier: HexString, data: Array[Byte]) = Request(
        url = URL(Root / "api" / "blobs"),
        method = Method.POST,
        body = Body.fromStream(
            Form(
                FormField.StreamingBinary(
                      name = "blob"
                    , data = ZStream.fromChunk(Chunk.fromArray(data))
                    , filename = Some(hash.toString())
                    , contentType = MediaType.application.`octet-stream`
                ),
                FormField.binaryField(name = "identifier", data = Chunk.fromArray(identifier.toByteArray), mediaType = MediaType.application.`octet-stream`)
                // FormField.StreamingBinary(
                //       name = "identifier"
                //     , data = ZStream.fromChunk(Chunk.fromArray(identifier.toByteArray))
                //     , filename = Some(identifier.toString())
                //     , contentType = MediaType.application.`octet-stream`
                // )
            )
            // , specificBoundary = Boundary(boundary)
            .multipartBytes(Boundary(boundary))
        ).contentType(newMediaType = MediaType.multipart.`form-data`, newBoundary = Boundary(boundary)),
        version = Version.Http_1_1,
    )

    val postEmptyForm = Request (
        url = URL(Root / "api" / "blobs"),
        method = Method.POST,
        body = Body.fromStream(
            Form.empty.multipartBytes(Boundary(boundary))
        ).contentType(newMediaType = MediaType.multipart.`form-data`, newBoundary = Boundary(boundary)),
        version = Version.Http_1_1,
    )

    def delete (hash: HexString, identifier: HexString) = Request(
        url = URL(Root / "api" / "blobs" / hash.toString()),
        method = Method.DELETE,
        body = Body.fromStream(
            Form(FormField.binaryField(
                  name = "identifier"
                , data = Chunk.fromArray(identifier.toByteArray)
                , mediaType = MediaType.application.`octet-stream`
            ))
            .multipartBytes(Boundary(boundary))
        ).contentType(newMediaType = MediaType.multipart.`form-data`, newBoundary = Boundary(boundary)),
        version = Version.Http_1_1,
    )

    def get (hash: HexString) = Request(
        url = URL(Root / "api" / "blobs" / hash.toString()),
        method = Method.GET,
        headers = Headers.empty,
        body = Body.empty,
        version = Version.Http_1_1,
        remoteAddress = None
    )

    // ========================================================================

    def spec = suite("BlobApis")(

        test("POST blob") {
            for {
                response <- app.runZIO(post(validBlobHash, identifier, validBlobData))
                body     <- response.body.asString

                fileExists      = Files.exists(Paths.get("target/tests/archive/blobs/f9032dd04636e22b80db4c8751395215/4b05df9bc15c6951a5a73d810e1c5cae"))
                metadataExists  = Files.exists(Paths.get("target/tests/archive/blobs/f9032dd04636e22b80db4c8751395215/4b05df9bc15c6951a5a73d810e1c5cae.metadata"))
            } yield allSuccesses(
                  assertTrue(response.status.code == 200)
                , assertTrue(fileExists)
                , assertTrue(metadataExists)
            // ,   assertTrue(body                 == validBlobHash.toString)
            )
        },

        // test("POST large blob") {
        //     for {
        //         response <- app.runZIO(post(largeBlobHash, identifier, largeBlobData))
        //         body     <- response.body.asString
        //     } yield allSuccesses(
        //         assertTrue(response.status.code == 200)
        //     // ,   assertTrue(body                 == validBlobHash.toString)
        //     )
        // } @@ TestAspect.timeout(10.second),

        test("GET blob") {
            for {
                postStatusCode <- app.runZIO(post(validBlobHash, identifier, validBlobData)).map(response => response.status.code)
                response       <- app.runZIO(get(validBlobHash))
                hash           <- app.runZIO(get(validBlobHash)).flatMap(response =>
                    response
                        .body.asStream
                        .run(ZSink.digest(MessageDigest.getInstance("SHA-256").nn))
                        .map((chunk: Chunk[Byte]) => HexString.bytesToHex(chunk.toArray))
                )
            } yield allSuccesses(
                assertTrue(postStatusCode        == 200)
            ,   assertTrue(response.status.code  == 200)
            ,   assertTrue(hash                  == validBlobHash)
            )
        },

        test("DELETE blob") {
            for {

                postStatusCode   <- app.runZIO(post(validBlobHash, identifier, validBlobData)).map(response => response.status.code)
                deleteStatusCode <- app.runZIO(delete(validBlobHash, identifier)).map(response => response.status.code)

                fileExistsAfter       = Files.exists(Paths.get("target/tests/archive/blobs/f9032dd04636e22b80db4c8751395215/4b05df9bc15c6951a5a73d810e1c5cae"))
                metadataExistsAfter   = Files.exists(Paths.get("target/tests/archive/blobs/f9032dd04636e22b80db4c8751395215/4b05df9bc15c6951a5a73d810e1c5cae.metadata"))
            } yield allSuccesses(
                  assertTrue(postStatusCode == 200)
                , assertTrue(deleteStatusCode  == 200)
                , assertTrue(fileExistsAfter == false)
                , assertTrue(metadataExistsAfter == false)
            )
        },

        test("POST blob - wrong filename") {
            for {
                statusCode <- app.runZIO(post(HexString("wrong"), identifier, validBlobData)).map(response => response.status.code)
            } yield assertTrue(statusCode == 400) 
        },

        test("POST blob - empty file") {
            for {
                statusCode <- app.runZIO(postEmptyForm).map(response => response.status.code)
            } yield assertTrue(statusCode == 400)
        },

        test("Get blob - missing") {
            for {
                postStatusCode   <- app.runZIO(post  (validBlobHash, identifier, validBlobData)).map(response => response.status.code)
                deleteStatusCode <- app.runZIO(delete(validBlobHash, identifier)).map(response => response.status.code)
                getStatusCode    <- app.runZIO(get(validBlobHash)).map(response => response.status.code)
            } yield allSuccesses(
              assertTrue(postStatusCode   == 200)
            , assertTrue(deleteStatusCode == 200)
            , assertTrue(getStatusCode    == 404)
            )
        },

        test("DELETE blob - wrong filename") {
            for {
                _          <- app.runZIO(post  (validBlobHash, identifier, validBlobData))     .map(response => response.status.code)
                statusCode <- app.runZIO(delete(HexString("wrong"), identifier)).map(response => response.status.code)
            } yield assertTrue(statusCode == 404)
        },

                test("DELETE blob - wrong identifier") {
            for {
                _          <- app.runZIO(post  (validBlobHash, identifier, validBlobData))     .map(response => response.status.code)
                statusCode <- app.runZIO(delete(validBlobHash, HexString("baab"))).map(response => response.status.code)
            } yield assertTrue(statusCode == 400)
        },

    ).provideLayerShared(environment)
    @@ TestAspect.sequential
    @@ TestAspect.timeout(30.second)
    @@ TestAspect.before   (ZIO.succeed(FileSystem.deleteAllFiles(blobBasePath.toFile().nn)))
    @@ TestAspect.beforeAll(ZIO.succeed(FileSystem.deleteAllFiles(blobBasePath.toFile().nn)))
    // @@ TestAspect.afterAll (ZIO.succeed(FileSystem.deleteAllFiles(blobBasePath.toFile().nn)))
