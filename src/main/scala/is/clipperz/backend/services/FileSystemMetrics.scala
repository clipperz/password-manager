package is.clipperz.backend.services

import java.nio.file.Path
import java.nio.file.Files
import java.io.File

import zio.metrics.{ Metric, MetricLabel }
import scala.jdk.StreamConverters.*
import scala.jdk.CollectionConverters.*

import zio.{ ZIO, Task }
import zio.Scheduler
import zio.Schedule

import zio.durationInt
import zio.metrics.MetricKeyType
import is.clipperz.backend.functions.elapsedTime
import zio.Chunk

def collectFileSystemMetrics(path: Path): Task[(Long, Long, Array[Long])] =
  elapsedTime("files", Set(MetricLabel("archive", path.getFileName().nn.toString())))(
    ZIO.log(s"Collect FileSystem Metrics [${path.toString}]")
    *>
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
