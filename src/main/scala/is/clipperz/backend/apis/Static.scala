package is.clipperz.backend.apis

import java.io.File
import zio.http.{ Method, Request, Path, Handler }
import zio.http.* //TODO: fix How do you import `!!` and `/`?
import is.clipperz.backend.Main.ClipperzHttpApp
import zio.metrics.{ Metric, MetricLabel }
import zio.ZIO
import is.clipperz.backend.functions.extractPath

val staticApi = Routes(
    Method.GET / "api" / "static" / trailing -> handler:
        Handler
        .param[(Path, Request)](_._1)
        .flatMap: fileName =>
            Handler
            .fromFile(File(s"target/output.webpack/${fileName}"))
            .tapZIO(r => ZIO.unit @@ Metric.frequency("static")
            .contramap(_ => fileName.encode))
)