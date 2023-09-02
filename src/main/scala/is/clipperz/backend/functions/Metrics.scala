package is.clipperz.backend.functions

import java.util.concurrent.TimeUnit.{ SECONDS, NANOSECONDS }

import zio.{ ZIO, Chunk }
import zio.durationInt
import zio.metrics.{ Metric, MetricLabel }
import zio.http.Response
import zio.http.Method

private val nanoToSeconds = 1e-9

def responseTimer[E](label: String, method: Method)(block: => ZIO[E, Throwable, Response]): ZIO[E, Throwable, Response] =

  def summary(t0: Long) = 
    ZIO.succeed((System.nanoTime() - t0).toDouble * nanoToSeconds)
      @@ Metric.summary(s"${label}.summary", 1.day, 100, 0.03d, Chunk(0.50, 0.75, 0.90, 0.95, 0.98 /*, 0.99, 0.999*/)).tagged("Method", method.name)
  
  def counters(r: Response) = 
    ZIO.succeed(r)
      @@ Metric.counter(s"${label}.success").contramap[Response](r => if r.status.isSuccess then 1L else 0L).tagged("Method", method.name)
      @@ Metric.counter(s"${label}.error"  ).contramap[Response](r => if r.status.isError   then 1L else 0L).tagged("Method", method.name).taggedWith(r => if r.status.isError then Set(MetricLabel("status", r.status.code.toString())) else Set.empty)
  
  def exceptions(t: Throwable) =
    ZIO.unit
      @@ Metric.counter(s"${label}.error").contramap[Any](_ => 1L).tagged("Method", method.name).tagged(MetricLabel("status", "500"))
  
  (for {
    t0     <- ZIO.succeed(System.nanoTime())
    result <- block.tapEither(_ => summary(t0))
                   .tapBoth  (exceptions, counters)
  } yield result)

def elapsedTime[E, R](label: String, tags: Set[MetricLabel])(block: => ZIO[E, Throwable, R]): ZIO[E, Throwable, R] =
  for {
    t0     <- ZIO.succeed(System.nanoTime())
    result <- block.tap(_ => ZIO.succeed((System.nanoTime() - t0).toDouble * nanoToSeconds)
                              @@ Metric.summary(s"${label}.elapsedTime", 1.day, 100, 0.03d, Chunk(0.50, 0.75, 0.90, 0.95, 0.98 /*, 0.99, 0.999*/)).tagged(tags)
                       )
  } yield result