package is.clipperz.backend.functions

import is.clipperz.backend.Exceptions.*
import java.time.format.DateTimeParseException
import zio.{ Cause, ZIO }
import zio.http.{Body, Response, Status }

def customErrorHandler(c: Cause[Throwable]): ZIO[Any, Nothing, Response] =
    val err = c.failureOption.getOrElse(c.dieOption.getOrElse(new Exception())) 

    ZIO.logWarningCause(s"${err.getMessage()}", c).as(customMapError(err))

def customMapError(err: Throwable) = Response(
    status = (err match {
        case    ( _: EmptyContentException
                | _: FailedConversionException
                | _: BadRequestException
                | _: NoSuchElementException
                )                                   =>  Status.BadRequest
        case    ( _: ResourceNotFoundException )    =>  Status.NotFound
        case    ( _: ResourceExpiredException )     =>  Status.Gone
        case    ( _: ResourceConflictException
                | _: ConflictualRequestException
                )                                   =>  Status.Conflict
        case    ( _: NonWritableArchiveException
                | _: NonReadableArchiveException
                | _: DateTimeParseException
                )                                   =>  Status.InternalServerError
        case    ( _ )                               =>  Status.InternalServerError
    }),
    body = Body.fromString(err.getMessage().nn)
)
