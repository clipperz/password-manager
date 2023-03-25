package is.clipperz.backend.exceptions

class NonReadableArchiveException(error: String) extends Exception(error):
  def this(message: String, cause: Throwable) =
    this(message)
    initCause(cause)
