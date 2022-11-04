package is.clipperz.backend

import zio.ZLogger
import zio.Trace
import zio.LogLevel
import zio.FiberId
import zio.Cause
import zio.FiberRefs
import zio.LogSpan
import zio.logging.LogFormat
import zio.logging.console
import zio.ZLayer
import zio.logging.LogColor
import zio.logging.LoggerNameExtractor
import java.time.format.DateTimeFormatter
import zio.logging.LogFilter
import zio.logging.LogGroup

object CustomLogger:
  val basicLogger: ZLogger[String, Unit] =
    new ZLogger[String, Unit]:
      override def apply(
        trace: Trace,
        fiberId: FiberId,
        logLevel: LogLevel,
        message: () => String,
        cause: Cause[Any],
        context: FiberRefs,
        spans: List[LogSpan],
        annotations: Map[String, String]
      ): Unit =
        val annotationsString = annotations.map((key, value) => s"\n\t- ${key}: ${value}").reduceOption(_ + _).getOrElse("")
        println(s"${java.time.Instant.now()} - ${logLevel.label} - ${message()} ${annotationsString}")
  
  def coloredLogger(minLevel: LogLevel): ZLogger[String, Unit] = 
    (
      LogFormat.timestamp(DateTimeFormatter.ISO_INSTANT.nn).color(LogColor.BLUE)  |-| LogFormat.text("-") |-|
      LogFormat.level |-| LogFormat.text("-") |-|
      LogFormat.line.color(LogColor.CYAN) +
      (LogFormat.space + LogFormat.text("-") |-| LogFormat.annotations.color(LogColor.WHITE)).filter(LogFilter(LogGroup((_, _, _, _, _, _, _, map) => !map.isEmpty)))
    )
    .filter(LogFilter.logLevel(minLevel))
    .toLogger
    .map(println(_))
