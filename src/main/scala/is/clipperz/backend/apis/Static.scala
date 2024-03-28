package is.clipperz.backend.apis

import java.io.File
import zio.http.{ Handler, Method, Path, Request, Routes, handler, trailing }
import is.clipperz.backend.Main.ClipperzHttpApp
import zio.metrics.{ Metric, MetricLabel }
import zio.ZIO

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