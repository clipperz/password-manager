package is.clipperz.backend.apis

import java.io.File
import zio.http.{ Http, Method, Request, Path, PathSyntax }
import zio.http.* //TODO: fix How do you import `!!` and `/`?
import is.clipperz.backend.Main.ClipperzHttpApp
import zio.metrics.{ Metric, MetricLabel }
import zio.ZIO

val staticApi: ClipperzHttpApp = Http.collectHttp[Request]:
  case request @ Method.GET -> Root / "api" / "static" / file =>
    Http.fromFile(File(s"target/output.webpack/${file}")).tapZIO(r => ZIO.unit @@ Metric.frequency("static").contramap(_ => file))
