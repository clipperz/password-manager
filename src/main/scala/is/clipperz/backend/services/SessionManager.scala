package is.clipperz.backend.services

import is.clipperz.backend.Exceptions.*
import is.clipperz.backend.data.HexString.bytesToHex

import java.util.concurrent.TimeUnit

import scala.collection.immutable.HashMap
import scala.concurrent.duration.fromNow

import zio.{ Duration, Ref, ZIO, Layer, ZLayer, Tag, Task, UIO, durationInt }
import zio.cache.{ Cache, Lookup }
import zio.internal.stacktracer.Tracer
import zio.http.Request

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

    private def extractSessionKey(request: Request): Task[SessionKey] =
        ZIO
        .attempt(request.rawHeader(SessionManager.sessionKeyHeaderName).get)
        .mapError(_ => new NoSuchElementException("session header key not found when deleting session"))

    private def getSessionKey(prng: PRNG, request: Request): Task[SessionKey] =
        extractSessionKey(request)
        .catchAll(_ => prng.nextBytes(32).map(bytesToHex(_).toString()))

    case class TrivialSessionManager(prng: PRNG) extends SessionManager:
        var sessions: Map[SessionKey, Session] = new HashMap[SessionKey, Session]()
        def emptySession(key: SessionKey) = Session(key, new HashMap[String, String]())

        override def getSession (request: Request): Task[Session] =
            getSessionKey(prng, request)
            .map(key => 
                sessions.getOrElse(key, emptySession(key))
            )

        override def saveSession (content: Session): Task[SessionKey] =
            sessions = sessions + ((content._1, content))
            ZIO.succeed(content._1)

        override def deleteSession (request: Request): Task[Unit] =
            extractSessionKey(request)
            .map(key => 
                sessions = sessions - key
            )
        
    val liveTrivial: ZLayer[PRNG, Throwable, SessionManager] =
        ZLayer.scoped(
            for {
                prng <- ZIO.service[PRNG]
            } yield TrivialSessionManager(prng)
        )

    case class ZioCacheSessionManager (prng: PRNG, sessions: Cache[String, Nothing, Ref[Session]]) extends SessionManager:
        
        private def refreshSessionTimeout(session: Session) =
            for {
                _       <- sessions.invalidate(session.key)
                ref     <- sessions.get(session.key)
                _       <- ref.set(session)
            } yield ()

        override def getSession (request: Request): Task[Session] =
            for {
                key     <- getSessionKey(prng, request)
                session <- sessions.get(key).flatMap(_.get)
                _       <- refreshSessionTimeout(session)
            } yield session

        override def saveSession (session: Session): Task[SessionKey] =
            refreshSessionTimeout(session).map(_ => session.key)

        override def deleteSession (request: Request): Task[Unit] =
            extractSessionKey(request)
            .flatMap (key => sessions.invalidate(key))

    def live(timeToLive: Duration = 10.minutes): ZLayer[PRNG, Throwable, SessionManager] =
        ZLayer.scoped(
            for {
                prng      <- ZIO.service[PRNG]
                sessions  <- Cache.make(capacity = 100, timeToLive = timeToLive, lookup = Lookup((key: SessionKey) => Ref.make(Session(key, HashMap.empty))))
            } yield ZioCacheSessionManager(prng, sessions)
        )
