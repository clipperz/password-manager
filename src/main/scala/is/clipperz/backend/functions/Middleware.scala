package is.clipperz.backend.functions

import zio.http.Request

def extractPath(req: Request): String =
    req.path.segments(1)
    