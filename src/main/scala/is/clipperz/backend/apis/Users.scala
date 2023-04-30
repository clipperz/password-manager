package is.clipperz.backend.apis

import zio.ZIO
import zio.json.EncoderOps
import zio.stream.ZStream
import zio.http.{ Http, Method, Response, Request, Status }
import zio.http.* //TODO: fix How do you import `!!` and `/`?
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
import is.clipperz.backend.functions.fromStream
import is.clipperz.backend.services.{ BlobArchive, SessionManager, SignupData, UserArchive, UserCard }
import is.clipperz.backend.Main.ClipperzHttpApp
import zio.Cause
import is.clipperz.backend.services.ModifyUserCard
import is.clipperz.backend.LogAspect

val usersApi: ClipperzHttpApp = Http.collectZIO[Request] {
  case request @ Method.POST -> !! / "users" / c =>
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
                  (userArchive.saveUser(signupData.user, false)
                    <&> // Returns an effect that executes both this effect and the specified effect, in parallel, combining their results into a tuple. If either side fails, then the other side will be interrupted.
                      blobArchive
                        .saveBlob(signupData.indexCardReference, ZStream.fromIterable(signupData.indexCardContent.toByteArray))
                      <&> // Returns an effect that executes both this effect and the specified effect, in parallel, combining their results into a tuple. If either side fails, then the other side will be interrupted.
                      blobArchive
                        .saveBlob(
                          signupData.preferencesReference,
                          ZStream.fromIterable(signupData.preferencesContent.toByteArray),
                        )
                      <&>
                      ZIO.foreach(signupData.cards) { (reference, content) =>
                        blobArchive.saveBlob(reference, ZStream.fromIterable(content.toByteArray))
                      })
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
      .catchSome {
        case ex: ResourceConflictException =>
          ZIO.logDebugCause(s"RESOURCE CONFLICT: ${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.Conflict))
        case ex: ConflictualRequestException =>
          ZIO.logDebugCause(s"CONFLICTUAL REQUEST: ${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.Conflict))
        case ex: BadRequestException =>
          ZIO.logFatalCause(s"BAD REQUEST: ${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
        case ex: NonWritableArchiveException =>
          ZIO.logFatalCause(s"NON WRITABLE ARCHIVE: ${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.InternalServerError))
        case ex: FailedConversionException =>
          ZIO.logWarningCause(s"FAILED CONVERSION: ${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
        case ex => ZIO.logFatalCause(s"${ex.getMessage()}", Cause.fail(ex)).flatMap(_ => ZIO.fail(ex))
      } @@ LogAspect.logAnnotateRequestData(request)

  case request @ Method.PUT -> !! / "users" / c =>
    ZIO
      .service[UserArchive]
      .zip(ZIO.service[BlobArchive])
      .zip(ZIO.service[SessionManager])
      .zip(ZIO.succeed(request.body.asStream))
      .flatMap((userArchive, blobArchive, sessionManager, content) =>
        sessionManager
          .verifySessionUser(c, request)
          .flatMap(_ =>
            userArchive
              .getUser(HexString(c))
              .flatMap(optionalUser =>
                optionalUser match
                  case Some(u) => ZIO.succeed(u)
                  case None => ZIO.fail(new ResourceNotFoundException(s"user ${c} does not exist"))
              )
          )
          .flatMap(_ =>
            fromStream[ModifyUserCard](content)
              .flatMap { modifyData =>
                if HexString(c) == modifyData.c then
                  userArchive
                    .getUser(modifyData.c)
                    .flatMap(currentUser =>
                      if currentUser.isDefined && currentUser.get == modifyData.oldUserCard then
                        userArchive.saveUser(modifyData.newUserCard, true)
                      else
                        ZIO.logDebug(s"${currentUser} is different than ${modifyData.oldUserCard}")
                        ZIO.fail(new BadRequestException("old user card is not the current one saved"))
                    )
                else ZIO.fail(new BadRequestException("c in request path differs from c in request body "))
              }
          )
      )
      .map(results => Response.text(results.toString))
      .catchSome {
        case ex: ResourceNotFoundException =>
          ZIO.logInfoCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.NotFound))
        case ex: BadRequestException =>
          ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
        case ex: NonWritableArchiveException =>
          ZIO.logFatalCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.InternalServerError))
        case ex: FailedConversionException =>
          ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
      } @@ LogAspect.logAnnotateRequestData(request)

  case request @ Method.GET -> !! / "users" / c =>
    ZIO
      .service[UserArchive]
      .zip(ZIO.service[SessionManager])
      .flatMap((userArchive, sessionManager) =>
        sessionManager
          .verifySessionUser(c, request)
          .flatMap(_ => userArchive.getUser(HexString(c)))
      )
      .map(maybeCard =>
        maybeCard match
          case None => Response(status = Status.NotFound)
          case Some(card) => Response.json(card.toJson)
      )
      .catchSome {
        case ex: NonReadableArchiveException =>
          ZIO.logFatalCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.InternalServerError))
        case ex: BadRequestException =>
          ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
      } @@ LogAspect.logAnnotateRequestData(request)

  case request @ Method.DELETE -> !! / "users" / c =>
    ZIO
      .service[UserArchive]
      .zip(ZIO.service[SessionManager])
      .zip(ZIO.succeed(request.body.asStream))
      .flatMap((userArchive, sessionManager, content) =>
        sessionManager
          .verifySessionUser(c, request)
          .flatMap(_ =>
            fromStream[UserCard](content)
              .flatMap(userCard => userArchive.deleteUser(userCard))
              .flatMap(b =>
                if b then
                  sessionManager.deleteSession(request).map(_ => Response.text(c))
                else ZIO.succeed(Response(status = Status.NotFound))
              )
          )
      )
      .catchSome {
        case ex: NonWritableArchiveException =>
          ZIO.logFatalCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.InternalServerError))
        case ex: BadRequestException =>
          ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
        case ex: ResourceNotFoundException =>
          ZIO.logInfoCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.NotFound))
      } @@ LogAspect.logAnnotateRequestData(request)
}
