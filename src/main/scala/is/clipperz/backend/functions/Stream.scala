package is.clipperz.backend.functions

import is.clipperz.backend.Exceptions.*

// import java.nio.charset.StandardCharsets

import zio.{ ZIO, Task }
import zio.json.{ JsonDecoder, DecoderOps }
import zio.stream.{ ZSink, ZStream }
import zio.nio.charset.Charset


def fromStream[A](using decoder: JsonDecoder[A])(content: ZStream[Any, Throwable, Byte]): Task[A] =
  content
    .run(ZSink.collectAll[Byte])
    .flatMap(chunk => Charset.Standard.utf8.decodeString(chunk))
    .flatMap(body => fromString(body))
    .foldZIO(
      err => ZIO.fail(new FailedConversionException(s"${err}")),
      res => ZIO.succeed(res),
    )

def fromString[A](using decoder: JsonDecoder[A])(string: String): Task[A] =
  string.fromJson[A] match
    case Left(error: String) => ZIO.fail(new FailedConversionException(s"JSON CONVERSION ERROR: ${error}"))
    case Right(value: A) => ZIO.succeed(value)
