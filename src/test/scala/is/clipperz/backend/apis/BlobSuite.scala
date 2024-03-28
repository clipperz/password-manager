package is.clipperz.backend.apis

import java.security.MessageDigest

import scala.language.postfixOps

import zio.nio.file.{ Files, Path, FileSystem }
import zio.{ Chunk, Task, ZIO, ZLayer, durationInt }
import zio.json.EncoderOps
import zio.http.{ Body, Boundary, Form, FormField, Headers, MediaType, Method, Request, Root, Server, URL, Version }
import zio.http.Server.RequestStreaming
import zio.stream.{ ZStream, ZSink }
import zio.test.{ ZIOSpecDefault, assertTrue, assert, TestAspect }
import zio.test.Assertion.{ nothing }
import zio.test.TestResult.{ allSuccesses }

import is.clipperz.backend.Main
import is.clipperz.backend.data.{ Base, HexString }
import is.clipperz.backend.data.HexString.bytesToHex
import is.clipperz.backend.functions.{ customErrorHandler, fromStream }
import is.clipperz.backend.functions.crypto.HashFunction
import is.clipperz.backend.services.{ BlobArchive, OneTimeShareArchive, PRNG, SessionManager, SrpManager, UserArchive, TollManager }
import is.clipperz.backend.TestUtilities
import zio.nio.charset.Charset


object BlobSpec extends ZIOSpecDefault:

    def spec = suite("BlobApis")(

        test("POST blob") {
            for {
                content     <- validBlobData
                response    <- app.runZIO(post(validBlobHash, identifier, content))
                body        <- response.body.asString

                fileExists      <- Files.exists(Path("target/tests/archive/blobs/f903/2dd0/4636/e22b/80db/4c87/5139/5215/4b05/df9b/c15c/6951/a5a7/3d81/0e1c/5cae/f9032dd04636e22b80db4c87513952154b05df9bc15c6951a5a73d810e1c5cae.blob"))
                metadataExists  <- Files.exists(Path("target/tests/archive/blobs/f903/2dd0/4636/e22b/80db/4c87/5139/5215/4b05/df9b/c15c/6951/a5a7/3d81/0e1c/5cae/f9032dd04636e22b80db4c87513952154b05df9bc15c6951a5a73d810e1c5cae.metadata"))
            } yield allSuccesses(
                  assertTrue(response.status.code == 200)
                , assertTrue(fileExists)
                , assertTrue(metadataExists)
            // ,   assertTrue(body                 == validBlobHash.toString)
            )
        },

        test("POST large blob") {
            // val file_signature = blob_1K    //  OK
            val file_signature = blob_8K    //  OK
            // val file_signature = blob_1M    //  fail
            for {
                content  <- readSampleBlob(file_signature)
                response <- app.runZIO(post(HexString(file_signature), identifier, content))
                body     <- response.body.asString
            } yield allSuccesses(
                assertTrue(response.status.code == 200)
            // ,   assertTrue(body                 == validBlobHash.toString)
            )
        } @@ TestAspect.timeout(2.second),

        test("GET blob") {
            for {
                content         <- validBlobData
                postStatusCode  <- app.runZIO(post(validBlobHash, identifier, content)).map(response => response.status.code)
                response        <- app.runZIO(get(validBlobHash))
                hash            <- app.runZIO(get(validBlobHash)).flatMap(response =>
                    response
                        .body.asStream
                        .run(ZSink.digest(MessageDigest.getInstance("SHA-256").nn))
                        .map((chunk: Chunk[Byte]) => HexString.bytesToHex(chunk.toArray))
                )
            } yield allSuccesses(
                  assertTrue(postStatusCode        == 200)
                , assertTrue(response.status.code  == 200)
                , assertTrue(hash                  == validBlobHash)
            )
        },

        test("DELETE blob") {
            for {
                content         <- validBlobData
                postResponse    <- app.runZIO(post(validBlobHash, identifier, content))
                deleteResponse  <- app.runZIO(delete(validBlobHash, identifier))

                fileExistsAfter         <- Files.exists(Path("target/tests/archive/blobs/f903/2dd0/4636/e22b/80db/4c87/5139/5215/4b05/df9b/c15c/6951/a5a7/3d81/0e1c/5cae/f9032dd04636e22b80db4c87513952154b05df9bc15c6951a5a73d810e1c5cae.blob"))
                metadataExistsAfter     <- Files.exists(Path("target/tests/archive/blobs/f903/2dd0/4636/e22b/80db/4c87/5139/5215/4b05/df9b/c15c/6951/a5a7/3d81/0e1c/5cae/f9032dd04636e22b80db4c87513952154b05df9bc15c6951a5a73d810e1c5cae.metadata"))
            } yield allSuccesses(
                  assertTrue(postResponse.status.code == 200)
                , assertTrue(deleteResponse.status.code  == 200)
                , assertTrue(fileExistsAfter == false)
                , assertTrue(metadataExistsAfter == false)
            )
        },

        test("POST blob - wrong filename") {
            for {
                content     <- validBlobData
                response    <- app.runZIO(post(HexString("wrong"), identifier, content))
                body        <- response.body.asString
                // statusCode  =  response.status.code
            } yield allSuccesses(
                  assertTrue(response.status.code == 400)
                , assertTrue(body == "Hash of content does not match with hash field provided")
            )
        },

        test("POST blob - empty file") {
            for {
                response    <- app.runZIO(postEmptyForm)
                body        <- response.body.asString
            } yield allSuccesses(
                  assertTrue(response.status.code == 400)
                , assertTrue(body == "Missing either/both 'blob', 'identifier' fields")
            )
        },

        test("Get blob - missing") {
            for {
                content         <- validBlobData
                postResponse    <- app.runZIO(post  (validBlobHash, identifier, content))
                deleteResponse  <- app.runZIO(delete(validBlobHash, identifier))
                getResponse     <- app.runZIO(get(validBlobHash))
                getBody         <- getResponse.body.asString
            } yield allSuccesses(
                  assertTrue(postResponse.status.code   == 200)
                , assertTrue(deleteResponse.status.code == 200)
                , assertTrue(getResponse.status.code    == 404)
                , assertTrue(getBody == "Referenced blob does not exists")
            )
        },

        test("DELETE blob - wrong filename") {
            for {
                content         <- validBlobData
                postResponse    <- app.runZIO(post  (validBlobHash, identifier, content))
                deleteResponse  <- app.runZIO(delete(HexString("wrong"), identifier))
                deleteBody      <- deleteResponse.body.asString
            } yield allSuccesses(
                  assertTrue(deleteResponse.status.code == 404)
                , assertTrue(deleteBody == "Referenced blob does not exists")
            )
        },

        test("DELETE blob - wrong identifier") {
            for {
                content         <- validBlobData
                postResponse    <- app.runZIO(post  (validBlobHash, identifier, content))
                deleteResponse  <- app.runZIO(delete(validBlobHash, HexString("baab")))
                deleteBody      <- deleteResponse.body.asString
            } yield allSuccesses(
                  assertTrue(deleteResponse.status.code == 400)
                , assertTrue(deleteBody == "Wrong blob identifier provided")
            )
        },

    ).provideLayerShared(environment)
    @@ TestAspect.sequential
    @@ TestAspect.timeout(30.second)
    @@ TestAspect.before   (TestUtilities.deleteFilesInFolder(blobBasePath))
    @@ TestAspect.beforeAll(TestUtilities.deleteFilesInFolder(blobBasePath))
    // @@ TestAspect.afterAll (TestUtilities.deleteFilesInFolder(blobBasePath))

    // ========================================================================

    val app =  (   blobsApi        
               ).handleErrorCauseZIO(customErrorHandler)
                .toHttpApp
    val blobBasePath = FileSystem.default.getPath("target", "tests", "archive", "blobs")
    val userBasePath = FileSystem.default.getPath("target", "tests", "archive", "users")
    val oneTimeShareBasePath = FileSystem.default.getPath("target", "tests", "archive", "one_time_share")

    val boundary = "--TestBoundary"

    val keyBlobArchiveFolderDepth = 16

    val environment =
        ZLayer.succeed(Server.Config.default.requestStreaming(RequestStreaming.Enabled)) ++
        PRNG.live ++
        (PRNG.live >>> SessionManager.live()) ++
        UserArchive.fs(userBasePath, keyBlobArchiveFolderDepth, false) ++
        BlobArchive.fs(blobBasePath, keyBlobArchiveFolderDepth, false) ++
        OneTimeShareArchive.fs(oneTimeShareBasePath, keyBlobArchiveFolderDepth, false) ++
        ((UserArchive.fs(userBasePath, keyBlobArchiveFolderDepth, false) ++ PRNG.live) >>> SrpManager.v6a()) ++
        (PRNG.live >>> TollManager.live)

    // val largeBlobData: Task[ZStream[Any, Nothing, Byte]] = Files.readAllBytes(Path("src/test/resources/blobs/4073041693a9a66983e6ffb75b521310d30e6db60afc0f97d440cb816bce7c63.blob")).map(ZStream.fromChunk)
    // val largeBlobHash = HexString("4073041693a9a66983e6ffb75b521310d30e6db60afc0f97d440cb816bce7c63")

    val blob_1K     = "0dfba6266bcebf53a0ed863f5df4edf56066e6a5194df242a2b31f13bf7bb9f8"
    val blob_2K     = "8960c75f721872b381f4e81ca7219bd268a47019e019264de3418088e4b1fbb0"
    val blob_3K     = "629c90e7b104b8d7de31bbc3aabd05305451b9684e1d5ab38e49ba7bb63f4396"
    val blob_4K     = "0d37b6cba13b88d803e866241fcace8b7f1dad25156f8f2383f914f3f54fc51e"
    val blob_5K     = "31bae7cdae1214f650a92f0731e7af0ed3d8805a0f883ef5031fd2b77c77f6c4"
    val blob_6K     = "6bb01cc243776c588ee34ee283043db4575ee1cf7fff838a1ead9f3ee8b785f0"
    val blob_7K     = "5d134b0d55a3efc8434849cbd8136ebf4730f33d6688b7397ef1a14d66c003ef"
    val blob_8K     = "35a2870c8031ff6eb2357611dde0cdab009105d9627858c7857d8c1d98f52a4c"
    val blob_9K     = "0c03d7fcf61a5b338f462a39f56f6556a106a68bd488604183c97f09b26724aa"

    val blob_10K    = "90a6bbdfc71693e18e021906479463d5d685fd3661ee00c91d31297968c36331"
    val blob_100K   = "36ae43f85e706511dbabc8dc38cc0b3fe737f9a5cf7c3d11b6e36889634073a2"
    val blob_200K   = "35d7eb0d88cbcad1779e592d3b0b59e61ab9818890283b3a1cb9cb32175d6733"
    val blob_300K   = "9ae5235637f049c02988d4a5cf5a321c7246a8b7bf133eeb69d11095e3bb2aad"
    val blob_400K   = "eaa8eea0ac6540ab1d021f436599f48a9f69bda37ea2841acd3ba1184dd639b4"

    val blob_1M     = "4073041693a9a66983e6ffb75b521310d30e6db60afc0f97d440cb816bce7c63"


    val validBlobHash = HexString("f9032dd04636e22b80db4c87513952154b05df9bc15c6951a5a73d810e1c5cae")
    // val validBlobData = Charset.Standard.utf8.encodeString("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nam ante massa, congue a sapien vel, efficitur facilisis eros. Mauris varius leo ut dolor malesuada, a pretium est scelerisque. Integer ut.").map(ZStream.fromChunk)
    val validBlobData = readSampleBlob("f9032dd04636e22b80db4c87513952154b05df9bc15c6951a5a73d810e1c5cae")
    val identifier    = HexString("affa")

    def readSampleBlob (blobHash: String): Task[ZStream[Any, Nothing, Byte]] = Files.readAllBytes(Path(s"src/test/resources/blobs/${blobHash}.blob")).map(ZStream.fromChunk)

    def post (hash: HexString, identifier: HexString, data: ZStream[Any, Nothing, Byte]) = Request(
        url = URL(Root / "api" / "blobs"),
        method = Method.POST,
        body = Body.fromStream(
            Form(
                FormField.binaryField(name = "identifier", data = Chunk.fromArray(identifier.toByteArray), mediaType = MediaType.application.`octet-stream`),
                FormField.StreamingBinary(
                      name = "blob"
                    , data = data //    ZStream.fromChunk(Chunk.fromArray(data))
                    , filename = Some(hash.toString())
                    , contentType = MediaType.application.`octet-stream`
                ),
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

