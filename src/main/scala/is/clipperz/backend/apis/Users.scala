package is.clipperz.backend.apis
import is.clipperz.backend.data.HexString
import is.clipperz.backend.exceptions.{
  NonWritableArchiveException,
  NonReadableArchiveException,
  FailedConversionException,
  BadRequestException,
  ResourceConflictException,
  ResourceNotFoundException,
  ConflictualRequestException,
}
import is.clipperz.backend.functions.{ fromStream, responseTimer }
import is.clipperz.backend.services.*
import is.clipperz.backend.Main.ClipperzHttpApp
import is.clipperz.backend.LogAspect

import zio.{ ZIO, Cause }
import zio.http.{ Method, Response, Request, Status }
import zio.http.*
import zio.json.EncoderOps
import zio.stream.ZStream
import is.clipperz.backend.data.Base

val usersApi: Routes[BlobArchive & UserArchive & SessionManager, Throwable] = Routes(
    Method.POST / "api" / "users" / string("c") -> handler: (c: String, request: Request) =>
        responseTimer("users", request.method)(
        ZIO
            .service[UserArchive]
            .zip(ZIO.service[BlobArchive])
            .zip(ZIO.service[SessionManager])
            .zip(ZIO.succeed(request.body.asStream))
            .flatMap((userArchive, blobArchive, sessionManager, content) =>
            userArchive
                .getUser(HexString(c))
                .flatMap(optionalUser =>
                optionalUser match
                    case Some(_) => ZIO.fail(new ConflictualRequestException("User already exists"))
                    case None    => ZIO.succeed(())
                )
                .flatMap(_ =>
                fromStream[SignupData](content)
                    .flatMap { signupData =>
                    if HexString(c) == signupData.user.c then
                        (userArchive.saveUser(remoteFromRequest(signupData.user), false)
                        <&> // Returns an effect that executes both this effect and the specified effect, in parallel, combining their results into a tuple. If either side fails, then the other side will be interrupted.
                            blobArchive
                            .saveBlob(signupData.indexCardReference, ZStream.fromIterable(signupData.indexCardContent.toByteArray))
                        <&>
                            blobArchive
                            .saveBlob(
                                signupData.preferencesReference,
                                ZStream.fromIterable(signupData.preferencesContent.toByteArray),
                            )
                        <&>
                            ZIO.foreach(signupData.cards) { (reference, content) =>
                            blobArchive.saveBlob(reference, ZStream.fromIterable(content.toByteArray))
                            }
                        )
                        .parallelErrors
                        .foldZIO(
                            err => ZIO.fail(new Exception(s"${err}")),
                            result => ZIO.succeed(result),
                        )
                    else ZIO.fail(new BadRequestException("c in request path differs from c in request body "))
                    }
                )
            )
            .map(results => Response.text(results._1.toString))
        ) @@ LogAspect.logAnnotateRequestData(request)
,
    Method.PUT / "api"  / "users" / string("c") -> handler: (c: String, request: Request) =>
        responseTimer("users", request.method)(
        ZIO
            .service[UserArchive]
            .zip(ZIO.service[BlobArchive])
            .zip(ZIO.service[SessionManager])
            .zip(ZIO.succeed(request.body.asStream))
            .flatMap((userArchive, blobArchive, sessionManager, content) =>
            sessionManager.getSession(request)
                .flatMap(session =>
                sessionManager.verifySessionUser(c, session) *>
                userArchive
                    .getUser(HexString(c))
                    .flatMap(optionalUser =>
                    optionalUser match
                        case Some(u) => ZIO.succeed(u)
                        case None => ZIO.fail(new ResourceNotFoundException(s"user ${c} does not exist"))
                    )
                )
                .flatMap(currentUser =>
                fromStream[RequestUserCard](content)
                    .flatMap { userCard =>
                        userArchive
                            .getUser(HexString(userCard.c.toString))
                            .flatMap(optionalUser =>
                            optionalUser match
                                case Some(_) => ZIO.fail(new ConflictualRequestException("User already exists"))
                                case None    => ZIO.succeed(())
                            )
                        *>
                        (if userCard.originMasterKey.contains(currentUser.masterKey(0)) 
                        then
                            (userArchive.saveUser(remoteFromRequest(userCard), true))
                            <&>
                            userArchive.deleteUser(HexString(c))
                        else
                            ZIO.fail(new BadRequestException("origin does not match"))
                        )
                    }
                )
            )
            .map(_ => Response.ok)
        ) @@ LogAspect.logAnnotateRequestData(request)
,  
    Method.PATCH / "api" / "users" / string("c") -> handler: (c: String, request: Request) =>
        responseTimer("users", request.method)(
        ZIO
            .service[UserArchive]
            .zip(ZIO.service[BlobArchive])
            .zip(ZIO.service[SessionManager])
            .zip(ZIO.succeed(request.body.asStream))
            .flatMap((userArchive, blobArchive, sessionManager, content) =>
            sessionManager.getSession(request)
                .flatMap(session =>
                sessionManager.verifySessionUser(c, session) *>
                userArchive
                    .getUser(HexString(c))
                    .flatMap(optionalUser =>
                    optionalUser match
                        case Some(u) => ZIO.succeed(u)
                        case None => ZIO.fail(new ResourceNotFoundException(s"user ${c} does not exist"))
                    )
                )
                .flatMap(currentUser =>
                fromStream[UserCard](content)
                    .flatMap { userCard =>
                    if userCard.originMasterKey == currentUser.masterKey(0) then
                        userArchive.saveUser(currentUser.copy(masterKey = userCard.masterKey), true)
                    else
                        ZIO.fail(new BadRequestException("origin does not match"))
                    }
                )
            )
            .map(_ => Response.ok)
        ) @@ LogAspect.logAnnotateRequestData(request)
,
    Method.GET / "api" / "users" / string("c") -> handler: (c: String, request: Request) =>
        responseTimer("user", request.method)(
            ZIO
            .service[UserArchive]
            .zip(ZIO.service[SessionManager])
            .flatMap((userArchive, sessionManager) =>
                sessionManager.getSession(request)
                .flatMap(session =>
                    sessionManager
                    .verifySessionUser(c, session)
                    .flatMap(_ => userArchive.getUser(HexString(c)))
                )
            )
            .map(maybeCard => maybeCard match
                case None       => Response(status = Status.NotFound)
                case Some(card) => Response.json(card.masterKey.toJson)
            )
        ) @@ LogAspect.logAnnotateRequestData(request)
,
    Method.DELETE / "api" / "users" / string("c") -> handler: (c: String, request: Request) =>
        responseTimer("users", request.method)(
        ZIO
            .service[UserArchive]
            .zip(ZIO.service[SessionManager])
            .flatMap((userArchive, sessionManager) =>
            sessionManager.getSession(request)
                .flatMap(session =>
                sessionManager
                    .verifySessionUser(c, session)
                    .flatMap(_ =>
                    userArchive.deleteUser(HexString(c))
                        .flatMap(b =>
                        if b 
                            then sessionManager.deleteSession(request)
                                .map(_ => Response.text(c))
                            else ZIO.succeed(Response(status = Status.NotFound))
                        )
                    )
                )
            )
        ) @@ LogAspect.logAnnotateRequestData(request)
)