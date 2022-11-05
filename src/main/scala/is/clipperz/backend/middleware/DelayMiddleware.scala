package is.clipperz.backend.middleware

import zio.ZIO
import zio.json.EncoderOps
import zhttp.http.{ Headers, Http, Middleware, Request, Response, Status }



type DelayMiddleware = HttpMiddleware[SessionManager, Throwable]

val delays: DelayMiddleware = Middleware
  .ifThenElse[Request](req => req.headers.hasHeader(SessionManager.sessionKeyHeaderName))(
    _ => presentSessionHeaderMiddleware,
    missingSessionHeaderMiddleware,
  )
  .when(verifySessionNecessity)
