package is.clipperz.backend.functions

import zio.{ ZIO, Task }
import zio.stream.{ ZSink, ZStream }
import java.nio.charset.StandardCharsets
import zio.json.{ JsonDecoder, DecoderOps }

def fromStream[A] (using decoder: JsonDecoder[A])(content: ZStream[Any, Throwable, Byte]): Task[A] =
    content
      .run(ZSink.collectAll[Byte])
      .flatMap(chunk =>
        String(chunk.toArray, StandardCharsets.UTF_8).fromJson[A] match
          case Left(error: String) => ZIO.fail(new Exception(s"JSON CONVERSION ERROR: ${error}"))
          case Right(value: A) => ZIO.succeed(value)
      )
