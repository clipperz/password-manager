package is.clipperz.backend.functions

import zio.Cause
import zio.ZIO
import zio.http.Response
import is.clipperz.backend.exceptions.*
import zio.http.Status
import java.time.format.DateTimeParseException
import zio.http.Body

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
