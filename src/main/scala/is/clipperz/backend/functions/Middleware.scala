package is.clipperz.backend.functions

import zio.http.Request

def extractPath(req: Request): String =
  if req.path.leadingSlash then req.path.dropTrailingSlash.drop(1).take(1).toString
  else req.path.dropTrailingSlash.take(1).toString
