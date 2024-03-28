package is.clipperz.backend

import org.slf4j.LoggerFactory

import java.time.format.DateTimeFormatter

import zio.{ Cause, FiberId, FiberRefs, LogLevel, LogSpan, Trace, ZLayer, ZLogger }
import zio.logging.{ LogColor, LogFormat, LoggerNameExtractor, LogFilter, LogGroup, console }

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
          annotations: Map[String, String],
        ): Unit =
        val annotationsString = annotations.map((key, value) => s"\n\t- ${key}: ${value}").reduceOption(_ + _).getOrElse("")
        val throwableMsg = 
          cause.defects
            .map(t => {
                  if (t.getMessage() != null)
                    t.getMessage().nn 
                  else t.getClass().toString() + "\n" + t.getStackTrace().nn.map(e => e.nn.toString()).reduce((s1, s2) => s1 + "\n" + s2)
                })
            .reduceOption((s1, s2) => s1 + " -- " + s2)
        val msg = 
          throwableMsg match
            case None => message()
            case Some(s) => s
        println(s"${java.time.Instant.now()} - ${logLevel.label} - ${msg} ${annotationsString}")

  def basicColoredLogger(minLevel: LogLevel): ZLogger[String, Unit] =
    new ZLogger[String, Unit]:
      override def apply(
          trace: Trace,
          fiberId: FiberId,
          logLevel: LogLevel,
          message: () => String,
          cause: Cause[Any],
          context: FiberRefs,
          spans: List[LogSpan],
          annotations: Map[String, String],
        ): Unit =
        if (logLevel >= minLevel)
          val annotationsString = annotations.map((key, value) => s"\n\t- ${key}: ${value}").reduceOption(_ + _).getOrElse("")
          val throwableMsg = 
            cause.defects
              .map(t => {
                    if (t.getMessage() != null)
                      t.getMessage().nn 
                    else t.getClass().toString() + "\n" + t.getStackTrace().nn.map(e => e.nn.toString()).reduce((s1, s2) => s1 + "\n" + s2)
                  })
              .reduceOption((s1, s2) => s1 + " -- " + s2)
          val msg = 
            throwableMsg match
              case None => message()
              case Some(s) => s
          println(Console.BLUE + s"${java.time.Instant.now()}" + Console.CYAN + s" - ${logLevel.label} - " + Console.WHITE + s"${msg} ${annotationsString}")

  def coloredLogger(minLevel: LogLevel): ZLogger[String, Unit] =
    (
      LogFormat.timestamp(DateTimeFormatter.ISO_INSTANT.nn).color(LogColor.BLUE) |-| LogFormat.text("-") |-|
        LogFormat.level |-| LogFormat.text("-") |-|
        LogFormat.line.color(LogColor.CYAN) +
        (LogFormat.space + LogFormat.text("-") |-| LogFormat.annotations.color(LogColor.WHITE)).filter(
          LogFilter(LogGroup((_, _, _, _, _, _, _, map) => !map.isEmpty))
        )
    )
      .filter(LogFilter.logLevel(minLevel))
      .toLogger
      .map(println(_))
