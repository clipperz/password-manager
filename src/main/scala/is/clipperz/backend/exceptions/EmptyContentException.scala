package is.clipperz.backend.exceptions

class EmptyContentException(error: String) extends Exception(error):
  def this(message: String, cause: Throwable) =
    this(message)
    initCause(cause)

  def this() =
    this("Content is not present")
