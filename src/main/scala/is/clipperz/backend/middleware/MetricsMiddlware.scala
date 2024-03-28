package is.clipperz.backend.middleware

// import java.nio.file.{ Files, Path }
import zio.nio.file.{ Files, Path }
import java.util.concurrent.TimeUnit.{ SECONDS, NANOSECONDS }

import scala.jdk.CollectionConverters.*

import zio.{ Clock, Duration, Chunk, RuntimeFlags, Schedule, Task, Trace, ZIO, durationInt }
import zio.metrics.{ Metric, MetricLabel, MetricKeyType }
import zio.http.{ Handler, HandlerAspect, Method, Middleware, RoutePattern, Response, Request, Routes }

private val nanoToSeconds = 1e-9

// METRICS MIDDLEWARE

private val defaultSummaryParameters = (1.day, 100, 0.03d)

private val defaultQuantiles = Chunk(0.50, 0.75, 0.90, 0.95, 0.98 /*, 0.99, 0.999*/)

def metrics(
    concurrentRequestsName: String = "http_concurrent_requests_total",
    totalRequestsName: String = "http_requests_total",
    requestDurationName: String = "http_request_duration_seconds",
    requestDurationParameters: (Duration, Int, Double) = defaultSummaryParameters,
    requestDurationQuantiles: Chunk[Double] = defaultQuantiles,
    extraLabels: Set[MetricLabel] = Set.empty,
)(implicit trace: Trace): Middleware[Any] =
    val requestsTotal: Metric.Counter[RuntimeFlags] = Metric.counterInt(totalRequestsName)
    val concurrentRequests: Metric.Gauge[Double]    = Metric.gauge(concurrentRequestsName)
    val requestDuration: Metric.Summary[Double]     = Metric.summary(requestDurationName, requestDurationParameters._1, requestDurationParameters._2, requestDurationParameters._3, requestDurationQuantiles)
    val nanosToSeconds: Double                      = 1e9d

    def labelsForRequest (routePattern: RoutePattern[?]): Set[MetricLabel] = Set(
        MetricLabel("method", routePattern.method.render),
        MetricLabel("path", routePattern.pathCodec.render),
    ) ++ extraLabels

    def labelsForResponse (res: Response): Set[MetricLabel] = Set(MetricLabel("status", res.status.code.toString))

    def report (
        start: Long,
        requestLabels: Set[MetricLabel],
        labels: Set[MetricLabel],
    )(implicit trace: Trace): ZIO[Any, Nothing, Unit] =
        for {
            _   <- requestsTotal.tagged(labels).increment
            _   <- concurrentRequests.tagged(requestLabels).decrement
            end <- Clock.nanoTime
            took = end - start
            _ <- requestDuration.tagged(labels).update(took / nanosToSeconds)
        } yield ()

    def aspect (routePattern: RoutePattern[?])(implicit trace: Trace): HandlerAspect[Any, Unit] =
        HandlerAspect.interceptHandlerStateful(Handler.fromFunctionZIO[Request] { req =>
            val requestLabels = labelsForRequest(routePattern)

            for {
                start <- Clock.nanoTime
                _     <- concurrentRequests.tagged(requestLabels).increment
            } yield ((start, requestLabels), (req, ()))
        })(Handler.fromFunctionZIO[((Long, Set[MetricLabel]), Response)] { case ((start, requestLabels), response) =>
            val allLabels = requestLabels ++ labelsForResponse(response)
            report(start, requestLabels, allLabels).as(response)
        })

    new Middleware[Any]:
        def apply[Env1, Err](routes: Routes[Env1, Err]): Routes[Env1, Err] =
        Routes.fromIterable(
            routes.routes.map(route => route.transform[Env1](_ @@ aspect(route.routePattern))),
        )

// STATIC METRICS

def elapsedTime[E, R](label: String, tags: Set[MetricLabel])(block: => ZIO[E, Throwable, R]): ZIO[E, Throwable, R] =
    for {
        t0     <- ZIO.succeed(System.nanoTime())
        result <- block.tap(_ =>    ZIO.succeed((System.nanoTime() - t0).toDouble * nanoToSeconds)
                                    @@ Metric.summary(s"${label}.elapsedTime", 1.day, 100, 0.03d, Chunk(0.50, 0.75, 0.90, 0.95, 0.98 /*, 0.99, 0.999*/)).tagged(tags)
                        )
    } yield result

def collectFileSystemMetrics (path: Path): Task[(Long, Long, Array[Long])] =
    // elapsedTime("files", Set(MetricLabel("archive", path.getFileName().nn.toString())))(
    elapsedTime("files", Set(MetricLabel("archive", path.filename.toFile.toString)))(
        // ZIO.attempt(Files.walk(path).nn.iterator().nn.asScala
        Files.walk(path)
            // .map(_.toFile().nn)
            // .filter(file => file.isFile() && !file.isHidden())
            .filterZIO(path => Files.isRegularFile(path).zip(Files.isHidden(path)).map((regular, hidden) => (regular && !hidden)))
            // .map(file => (1, file.length()))
            .mapZIO(path => Files.size(path).map(size => (1, size)))
            //  count, totalSize, [size]
            .runFold((0L, 0L, Array.empty[Long]))((acc, tuple) => ((acc._1 + tuple._1), (acc._2 + tuple._2), acc._3 :+ (tuple._2)))
            @@ Metric.counter("files.count")
                .contramap[(Long, Long, Array[Long])](_._1)
                // .tagged("archive", path.getFileName().nn.toString())
                .tagged("archive", path.filename.toFile.toString)
            @@ Metric.counter("files.size")
                .contramap[(Long, Long, Array[Long])](_._2/1000)
                .tagged("archive", path.filename.toFile.toString)
    )

def scheduledFileSystemMetricsCollection (path: Path) =
    collectFileSystemMetrics(path) `repeat` Schedule.fixed(30.minutes)