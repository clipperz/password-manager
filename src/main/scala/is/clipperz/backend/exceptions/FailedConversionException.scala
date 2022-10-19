package is.clipperz.backed.exceptions

class FailedConversionException(error: String) extends Exception(error):
  def this(message: String, cause: Throwable) =
    this(message)
    initCause(cause)
