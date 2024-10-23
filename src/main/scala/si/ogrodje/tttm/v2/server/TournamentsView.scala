package si.ogrodje.tttm.v2.server

import si.ogrodje.tttm.v2.TournamentResults
import si.ogrodje.tttm.v2.persistance.DB.TransactorTask
import zio.ZIO
import zio.http.{Request, Response}
import doobie.implicits.*
import doobie.*
import zio.interop.catz.*

object TournamentsView:
  def lastTournaments(request: Request): ZIO[TransactorTask, Throwable, Response] = for
    tx     <- ZIO.service[TransactorTask]
    result <- sql"""SELECT version()""".query[String].to[List].transact(tx)
    out     = Response.text(s"PG version: ${result.mkString(", ")}")
  yield out
