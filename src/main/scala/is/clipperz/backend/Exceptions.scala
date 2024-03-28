package is.clipperz.backend

object Exceptions:
    class BadRequestException(error: String) extends Exception(error):
        def this(message: String, cause: Throwable) =
            this(message)
            initCause(cause)

    class ConflictualRequestException(error: String) extends Exception(error):
        def this(message: String, cause: Throwable) =
            this(message)
            initCause(cause)

    class EmptyContentException(error: String) extends Exception(error):
        def this(message: String, cause: Throwable) =
            this(message)
            initCause(cause)

        def this() =
            this("Content is not present")

    class FailedConversionException(error: String) extends Exception(error):
        def this(message: String, cause: Throwable) =
            this(message)
            initCause(cause)

    class NonReadableArchiveException(error: String) extends Exception(error):
        def this(message: String, cause: Throwable) =
            this(message)
            initCause(cause)

    class NonWritableArchiveException(error: String) extends Exception(error):
        def this(message: String, cause: Throwable) =
            this(message)
            initCause(cause)

    class ResourceConflictException(error: String) extends Exception(error):
        def this(message: String, cause: Throwable) =
            this(message)
            initCause(cause)

    class ResourceExpiredException(error: String) extends Exception(error):
        def this(message: String, cause: Throwable) =
            this(message)
            initCause(cause)

    class ResourceNotFoundException(error: String) extends Exception(error):
        def this(message: String, cause: Throwable) =
            this(message)
            initCause(cause)
