package is.clipperz.backend

import zio.{ Trace, UIO, ZIOAspect, ZIO }
import zio.http.Request

object LogAspect:
  def logAnnotateRequestData(req: Request): ZIOAspect[Nothing, Any, Nothing, Any, Nothing, Any] =
    new ZIOAspect[Nothing, Any, Nothing, Any, Nothing, Any]:
      override def apply[R, E, A](zio: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
        requestData(req).flatMap(data => ZIO.logAnnotate("request", data)(zio))

      def requestData(req: Request): UIO[String] =
        ZIO
          .succeed(req.method.toString())
          .zip(ZIO.succeed(req.path.toString))
          .map((method, path) => s"${method} ${path}")
