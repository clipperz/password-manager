package is.clipperz.backend.services

import scala.collection.immutable.HashMap
import zio.{ ZIO, Layer, ZLayer, Tag, Task }
import zio.internal.stacktracer.Tracer
import zio.http.Request
import is.clipperz.backend.exceptions.BadRequestException

type SessionKey = String
type SessionContent = Map[String, String]

case class Session(val key: SessionKey, val content: SessionContent):
  def `+`(contentTuple: (String, String)): Session =
    Session(key, content + contentTuple)
  def apply(contentKey: String): Option[String] =
    content.get(contentKey)
  def isEmpty: Boolean =
    content.isEmpty

trait SessionManager:
  def getSession(key: SessionKey): Task[Session]
  def saveSession(content: Session): Task[SessionKey]
  def verifySessionUser(c: String, request: Request): Task[Unit] =
    request.rawHeader(SessionManager.sessionKeyHeaderName) match
      case Some(sessionKey) =>
        this
          .getSession(sessionKey)
          .flatMap(session =>
            session("c") match
              case Some(session_c) =>
                if (session_c == c) ZIO.succeed(())
                else ZIO.fail(new BadRequestException("c in request path differs from c in session"))
              case None => ZIO.fail(new BadRequestException("session does not contain c"))
          )
      case None => ZIO.fail(new BadRequestException("session key not found in header"))
  def deleteSession(key: SessionKey): Task[Unit]

object SessionManager:
  val sessionKeyHeaderName = "clipperz-usersession-id"

  case class TrivialSessionManager(/* */ ) extends SessionManager:
    var sessions: Map[SessionKey, Session] = new HashMap[SessionKey, Session]()
    def emptySession(key: SessionKey) = Session(key, new HashMap[String, String]())

    override def getSession(key: SessionKey): Task[Session] =
      ZIO.succeed(sessions.getOrElse(key, emptySession(key)))

    override def saveSession(content: Session): Task[SessionKey] =
      sessions = sessions + ((content._1, content))
      ZIO.succeed(content._1)

    override def deleteSession(key: SessionKey): Task[Unit] =
      sessions = sessions - key
      ZIO.succeed(())

  val live: Layer[Nothing, SessionManager] =
    ZLayer.succeed[SessionManager](new TrivialSessionManager)(Tag[SessionManager], Tracer.newTrace)
