package is.clipperz.backend.apis

import java.io.File
import zhttp.http.{ Http, Method, Request }
import zhttp.http.* //TODO: fix How do you import `!!` and `/`?
import is.clipperz.backend.Main.ClipperzHttpApp

val staticApi = Http.collectHttp[Request] { 
  case request @ Method.GET -> !! / file =>
    Http.fromFile(File(s"target/output.parcel/${file}"))
}
