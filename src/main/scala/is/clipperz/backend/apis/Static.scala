package is.clipperz.backend.apis

import java.io.File
import zio.http.{ Http, Method, Request, Path, PathSyntax }
import zio.http.* //TODO: fix How do you import `!!` and `/`?
import is.clipperz.backend.Main.ClipperzHttpApp

val staticApi: ClipperzHttpApp = Http.collectHttp[Request] {
  case request @ Method.GET -> !! / file =>
    Http.fromFile(File(s"target/output.webpack/${file}"))
}
