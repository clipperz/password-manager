package is.clipperz.backend.apis

import zio.{ ZIO }
import zio.json.EncoderOps
import zio.stream.{ ZStream }
import zhttp.http.{ Http, Method, Response, Status }
import zhttp.http.* //TODO: fix How do you import `!!` and `/`?
import is.clipperz.backend.data.HexString
import is.clipperz.backend.functions.{ fromStream }
import is.clipperz.backend.services.{
  BlobArchive,
  SessionManager,
  SignupData,
  UserArchive,
  UserCard
}
import is.clipperz.backend.Main.ClipperzHttpApp

val usersApi: ClipperzHttpApp = Http.collectZIO {
  case request @ Method.POST -> !! / "users" / c =>
    ZIO.service[UserArchive]
      .zip(ZIO.service[BlobArchive])
      .zip(ZIO.service[SessionManager])
      .zip(ZIO.succeed(request.bodyAsStream))
      .flatMap((userArchive, blobArchive, sessionManager, content) =>
        userArchive.getUser(HexString(c)).flatMap(optionalUser => optionalUser match
          case Some(_) => sessionManager.verifySessionUser(c, request)
          case None    => ZIO.succeed(())
        ).flatMap(_ =>
          fromStream[SignupData](content)
          .flatMap(signupData => {
            if HexString(c) == signupData.user.c then
              ( userArchive.saveUser(signupData.user, false)
                <&> //Returns an effect that executes both this effect and the specified effect, in parallel, combining their results into a tuple. If either side fails, then the other side will be interrupted.
                blobArchive.saveBlob(signupData.indexCardReference, ZStream.fromIterable(signupData.indexCardContent.toByteArray))
                <&>
                ZIO.foreach(signupData.cards) { (reference, content) => blobArchive.saveBlob(reference, ZStream.fromIterable(content.toByteArray)) }
              ).parallelErrors.foldZIO(
                  _      => ZIO.fail(new Exception("TODO")),
                  result => ZIO.succeed(result)
              )                
            else  
              ZIO.fail(new Exception("c in request path differs from c in request body "))
          })
        )
      ).fold(
        err => { println(s"ERROR ${err}"); Response(status = Status.Conflict) },
        results => Response.text(results._1.toString)
      )

  case request @ Method.PUT -> !! / "users" / c =>
    ZIO.service[UserArchive]
      .zip(ZIO.service[BlobArchive])
      .zip(ZIO.service[SessionManager])
      .zip(ZIO.succeed(request.bodyAsStream))
      .flatMap((userArchive, blobArchive, sessionManager, content) =>
        userArchive.getUser(HexString(c)).flatMap(optionalUser => optionalUser match
          case Some(_) => sessionManager.verifySessionUser(c, request)
          case None    => ZIO.fail(new Exception(s"user ${c} does not exist"))
        ).flatMap(_ =>
          fromStream[UserCard](content)
          .flatMap(user => {
            if HexString(c) == user.c then
              userArchive.saveUser(user, true)
                .foldZIO(
                  _      => ZIO.fail(new Exception("TODO")),
                  result => ZIO.succeed(result)
                )                
            else  
              ZIO.fail(new Exception("c in request path differs from c in request body "))
          })
        ) 
      ).fold(
        err => { println(s"ERROR ${err}"); Response(status = Status.Conflict) }, // TODO: check errors
        results => Response.text(results._1.toString)
      )

  case request @ Method.GET -> !! / "users" / c =>
    ZIO.service[UserArchive]
      .zip(ZIO.service[SessionManager])
      .flatMap((userArchive, sessionManager) =>
        sessionManager.verifySessionUser(c, request)
        .flatMap(_ => userArchive.getUser(HexString(c)))
      )
      .map(maybeCard => 
        maybeCard match
          case None => Response(status = Status.NotFound)
          case Some(card) => Response.json(card.toJson)
      ).catchAll(
        err => { println(s"ERROR ${err}"); ZIO.succeed(Response(status = Status.NotFound)) }, // TODO: check errors
      )

  case request @ Method.DELETE -> !! / "users" / c =>
    ZIO.service[UserArchive]
      .zip(ZIO.service[SessionManager])
      .flatMap((userArchive, sessionManager) =>
        sessionManager.verifySessionUser(c, request)
        .flatMap(_ => userArchive.deleteUser(HexString(c)))
      )
      .map(_ => Response.text(c))
}
