package is.clipperz.backend.functions

import scala.jdk.CollectionConverters.*

import java.util.concurrent.TimeUnit.{ SECONDS, NANOSECONDS }
import java.nio.file.Path
import java.nio.file.Files

import zio.{ ZIO, Chunk, Task, Schedule }
import zio.durationInt
import zio.metrics.{ Metric, MetricLabel }
import zio.http.{ Response, Method }

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

def collectFileSystemMetrics(path: Path): Task[(Long, Long, Array[Long])] =
  elapsedTime("files", Set(MetricLabel("archive", path.getFileName().nn.toString())))(
    ZIO.attempt(Files.walk(path).nn.iterator().nn.asScala
       .map(_.toFile().nn)
       .filter(file => file.isFile() && !file.isHidden())
       .map(file => (1, file.length()))
       .foldLeft((0L, 0L, Array.empty[Long]))((acc, tuple) => (acc._1 + tuple._1, acc._2 + tuple._2, acc._3 :+ (tuple._2))))
      @@ Metric.counter("files.count")
               .contramap[(Long, Long, Array[Long])](_._1)
               .tagged("archive", path.getFileName().nn.toString())
      @@ Metric.counter("files.size")
               .contramap[(Long, Long, Array[Long])](_._2/1000)
               .tagged("archive", path.getFileName().nn.toString())
  )

def scheduledFileSystemMetricsCollection(path: Path) =
  collectFileSystemMetrics(path) `repeat` Schedule.fixed(30.minutes)