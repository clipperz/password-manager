package is.clipperz.backend.apis

import is.clipperz.backend.data.{ HexString, Base }
import is.clipperz.backend.Exceptions.*
import is.clipperz.backend.functions.{ fromStream }
import is.clipperz.backend.Main.ClipperzHttpApp
import is.clipperz.backend.middleware.authorizedMiddleware
import is.clipperz.backend.LogAspect
import is.clipperz.backend.services.{BlobArchive, RequestUserCard, SessionManager, SignupData, UserArchive, UserCard, remoteFromRequest}

import zio.{ ZIO, Cause }
import zio.http.{ Method, Response, Request, Routes, Status, handler, string }
import zio.json.EncoderOps
import zio.stream.ZStream
import is.clipperz.backend.services.CardsSignupData

val usersApi: Routes[BlobArchive & UserArchive & SessionManager, Throwable] = Routes(
    Method.POST / "api" / "users" / string("c") -> (handler: (c: String, request: Request) =>
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
                    // Returns an effect that executes both this effect and the specified effect, in parallel, combining their results into a tuple. If either side fails, then the other side will be interrupted.
                    (   userArchive.saveUser(remoteFromRequest(signupData.user), false)
                    <&> blobArchive.saveBlob(signupData.indexCardReference, signupData.indexCardIdentifier, ZStream.fromIterable(signupData.indexCardContent.toByteArray))
                    <&> blobArchive.saveBlob(signupData.userInfoReference,  signupData.userInfoIdentifier,  ZStream.fromIterable(signupData.userInfoContent.toByteArray))
                    <&> ZIO.foreach(signupData.cards) {
                            cardsSignupData => blobArchive.saveBlob(cardsSignupData.cardReference, cardsSignupData.cardIdentifier, ZStream.fromIterable(cardsSignupData.cardContent.toByteArray))
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
        .map(results => Response.text(results._1.toString)) @@ LogAspect.logAnnotateRequestData(request)
        )//.tapZIO(_ => ZIO.log("POST USER"))
) ++
Routes( 
    Method.PUT / "api"  / "users" / string("c") -> handler: (c: String, request: Request) =>
        ZIO
        .service[UserArchive]
        .zip(ZIO.service[BlobArchive])
        .zip(ZIO.succeed(request.body.asStream))
        .flatMap((userArchive, blobArchive, content) =>
            userArchive
            .getUser(HexString(c))
            .flatMap(optionalUser =>
            optionalUser match
                case Some(u) => ZIO.succeed(u)
                case None => ZIO.fail(new ResourceNotFoundException(s"user ${c} does not exist"))
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
        .map(_ => Response.ok) @@ LogAspect.logAnnotateRequestData(request)
,  
    Method.PATCH / "api" / "users" / string("c") -> handler: (c: String, request: Request) =>
        ZIO
        .service[UserArchive]
        .zip(ZIO.service[BlobArchive])
        .zip(ZIO.succeed(request.body.asStream))
        .flatMap((userArchive, blobArchive, content) =>
            userArchive
            .getUser(HexString(c))
            .flatMap(optionalUser =>
            optionalUser match
                case Some(u) => ZIO.succeed(u)
                case None => ZIO.fail(new ResourceNotFoundException(s"user ${c} does not exist"))
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
        .map(_ => Response.ok) @@ LogAspect.logAnnotateRequestData(request)
,
    Method.GET / "api" / "users" / string("c") -> handler: (c: String, request: Request) =>
        (for {
            userArchive  <- ZIO.service[UserArchive]
            optionalUser <- userArchive.getUser(HexString(c))
        } yield (optionalUser match
            case None       => Response(status = Status.NotFound)
            case Some(card) => Response.json(card.masterKey.toJson)
        )) @@ LogAspect.logAnnotateRequestData(request)
,
    Method.DELETE / "api" / "users" / string("c") -> handler: (c: String, request: Request) =>
        (for {
            userArchive    <- ZIO.service[UserArchive]
            sessionManager <- ZIO.service[SessionManager]
            _              <- userArchive.deleteUser(HexString(c))
            result         <- ZIO.succeed(true) //  TODO: fix this hack: Giulio Cesare [26-02-2024]
            _              <- sessionManager.deleteSession(request)
        } yield (if result
            then Response.text(c)
            else Response(status = Status.NotFound)
        )) @@ LogAspect.logAnnotateRequestData(request)
) @@ authorizedMiddleware(req => ZIO.attempt(req.path.segments.last))