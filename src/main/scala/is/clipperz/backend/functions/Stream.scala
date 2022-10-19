package is.clipperz.backend.functions

import zio.{ ZIO, Task }
import zio.stream.{ ZSink, ZStream }
import java.nio.charset.StandardCharsets
import zio.json.{ JsonDecoder, DecoderOps }

import is.clipperz.backed.exceptions.{FailedConversionException}

def fromStream[A] (using decoder: JsonDecoder[A])(content: ZStream[Any, Throwable, Byte]): Task[A] =
    content
      .run(ZSink.collectAll[Byte])
      .flatMap(chunk =>
        fromString(String(chunk.toArray, StandardCharsets.UTF_8))
      )
      .foldZIO(
        err => ZIO.fail(new FailedConversionException(s"${err}"))
      , res => ZIO.succeed(res)
      )

def fromString[A] (using decoder: JsonDecoder[A])(string: String): Task[A] =
  string.fromJson[A] match
    case Left(error: String) => ZIO.fail(new FailedConversionException(s"JSON CONVERSION ERROR: ${error}"))
    case Right(value: A) => ZIO.succeed(value)
