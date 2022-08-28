package is.clipperz.backend.services

import scala.collection.immutable.HashMap
import zio.{ ZIO, Layer, ZLayer, Tag, Task }
import zio.internal.stacktracer.Tracer
import zhttp.http.Request

type SessionKey = String
type SessionContent = Map[String, String]

case class Session (val key: SessionKey, val content: SessionContent):
  def `+`(contentTuple: (String, String)): Session =
    Session(key, content + contentTuple)
  def apply(contentKey: String): Option[String] =
    content.get(contentKey)

trait SessionManager:
  def getSession(key: SessionKey): Task[Session]
  def saveSession(content: Session): Task[SessionKey]
  def verifySessionUser(c: String, request: Request): Task[Unit] =
    request.headers.headerValue(SessionManager.sessionKeyHeaderName) match
      case Some(sessionKey) => this.getSession(sessionKey).flatMap(session =>
        session("c") match
          case Some(session_c) => if (session_c == c) ZIO.succeed(()) else ZIO.fail(new Exception("c in request path differs from c in session"))
          case None => ZIO.fail(new Exception("session does not contain c"))
      )
      case None => ZIO.fail(new Exception("session key not found in header"))
      
object SessionManager:
  val sessionKeyHeaderName = "clipperz-UserSession-ID"

  case class TrivialSessionManager(/* */) extends SessionManager:
    var sessions: Map[SessionKey, Session] = new HashMap[SessionKey, Session]()
    def emptySession(key: SessionKey) = Session(key, new HashMap[String, String]())
    
    override def getSession(key: SessionKey): Task[Session] =
      ZIO.succeed(sessions.getOrElse(key, emptySession(key)))

    override def saveSession(content: Session): Task[SessionKey] =
      sessions = sessions + ((content._1, content))
      ZIO.succeed(content._1)

  val live: Layer[Nothing, SessionManager] =
    ZLayer.succeed[SessionManager](new TrivialSessionManager)(Tag[SessionManager], Tracer.newTrace)
