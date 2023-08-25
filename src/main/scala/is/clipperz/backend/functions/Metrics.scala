package is.clipperz.backend.functions

import java.util.concurrent.TimeUnit.{ SECONDS, NANOSECONDS }

import zio.{ ZIO, Chunk }
import zio.durationInt
import zio.metrics.{ Metric, MetricLabel }
import zio.http.Response

def responseTimer[E](label: String)(block: => ZIO[E, Throwable, Response]): ZIO[E, Throwable, Response] =
  val nanoToSeconds = 1e-9

  def summary(t0: Long) = 
    ZIO.succeed((System.nanoTime() - t0).toDouble * nanoToSeconds)
      @@ Metric.summary(s"${label}.summary", 1.day, 100, 0.03d, Chunk(0.50, 0.75, 0.90, 0.95, 0.98 /*, 0.99, 0.999*/))
  
  def counters(r: Response) = 
    ZIO.succeed(r)
      @@ Metric.counter(s"${label}.success").contramap[Response](r => if r.status.isSuccess then 1L else 0L)
      @@ Metric.counter(s"${label}.error"  ).contramap[Response](r => if r.status.isError   then 1L else 0L).taggedWith(r => if r.status.isError then Set(MetricLabel("status", r.status.code.toString())) else Set.empty)
  
  def exceptions(t: Throwable) =
    ZIO.unit
      @@ Metric.counter(s"${label}.error").contramap[Any](_ => 1L).tagged(MetricLabel("status", "500"))
  
  (for {
    t0          <- ZIO.succeed(System.nanoTime())
    result      <- block.tapEither(_ => summary(t0))
                        .tapBoth  (exceptions, counters)
  } yield result)
