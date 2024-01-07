package is.clipperz.backend.services

import scala.collection.immutable.HashMap
import zio.{ ZIO, Layer, ZLayer, Tag, Task }
import zio.internal.stacktracer.Tracer
import zio.http.Request
import is.clipperz.backend.exceptions.BadRequestException
import is.clipperz.backend.data.HexString.bytesToHex
import zio.cache.Cache
import java.util.concurrent.TimeUnit
import java.time.Duration
import zio.cache.Lookup
import zio.Ref
import scala.concurrent.duration.fromNow

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
  def getSession(request: Request): Task[Session]
  def saveSession(content: Session): Task[SessionKey]
  def verifySessionUser(c: String, session: Session): Boolean =
    session("c") match
      case Some(session_c) => session_c == c
      case None => false
  def deleteSession(request: Request): Task[Unit]

object SessionManager:
  val sessionKeyHeaderName = "clipperz-usersession-id"

  case class TrivialSessionManager(prng: PRNG) extends SessionManager:
    var sessions: Map[SessionKey, Session] = new HashMap[SessionKey, Session]()
    def emptySession(key: SessionKey) = Session(key, new HashMap[String, String]())

    override def getSession (request: Request): Task[Session] =
      ZIO
        .attempt(request.rawHeader(SessionManager.sessionKeyHeaderName).get)
        .catchAll(_ => prng.nextBytes(32).map(bytesToHex(_).toString()))
        .map(key => sessions.getOrElse(key, emptySession(key)))

    override def saveSession (content: Session): Task[SessionKey] =
      sessions = sessions + ((content._1, content))
      ZIO.succeed(content._1)

    override def deleteSession (request: Request): Task[Unit] =
      ZIO.attempt(request.rawHeader(SessionManager.sessionKeyHeaderName).get)
         .map(key => sessions = sessions - key)
         .mapError(_ => new NoSuchElementException("session header key not found when deleting session"))

  case class ZioCacheSessionManager (prng: PRNG, sessions: Cache[String, Nothing, Ref[Session]]) extends SessionManager:
    override def getSession (request: Request): Task[Session] =
      ZIO
        .attempt (request.rawHeader(SessionManager.sessionKeyHeaderName).get)
        .catchAll(_   => prng.nextBytes(32).map(bytesToHex(_).toString()))
        .flatMap (key => sessions.get(key))
        .flatMap (_.get)

    override def saveSession (content: Session): Task[SessionKey] =
      sessions.refresh(content.key)

      sessions
        .get(content.key)
        .flatMap(ref => ref.set(content))
        .flatMap(_   => ZIO.succeed(content.key))

    override def deleteSession (request: Request): Task[Unit] =
      ZIO
        .attempt (request.rawHeader(SessionManager.sessionKeyHeaderName).get)
        .catchAll(_   => prng.nextBytes(32).map(bytesToHex(_).toString()))
        .flatMap (key => sessions.invalidate(key))


  val liveTrivial: ZLayer[PRNG, Throwable, SessionManager] =
    ZLayer.scoped(
        for {
            prng <- ZIO.service[PRNG]
        } yield TrivialSessionManager(prng)
    )

  val live: ZLayer[PRNG, Throwable, SessionManager] =
    ZLayer.scoped(
      for {
        prng      <- ZIO.service[PRNG]
        sessions  <- Cache.make(capacity = 100, timeToLive = Duration.ofMinutes(10).nn, lookup = Lookup((key: SessionKey) => Ref.make(Session(key, new HashMap[String, String]))))
      } yield ZioCacheSessionManager(prng, sessions)
    )
