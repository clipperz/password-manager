package is.clipperz.backend.exceptions

class NonWritableArchiveException(error: String) extends Exception(error):
  def this(message: String, cause: Throwable) =
    this(message)
    initCause(cause)
