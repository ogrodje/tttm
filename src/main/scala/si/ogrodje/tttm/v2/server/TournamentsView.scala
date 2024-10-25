package si.ogrodje.tttm.v2.server

import si.ogrodje.tttm.v2.{TournamentID, TournamentResults}
import si.ogrodje.tttm.v2.persistance.DB.TransactorTask
import zio.{RIO, ZIO}
import zio.http.{Request, Response}
import doobie.implicits.*
import doobie.*
import si.ogrodje.tttm.v2.persistance.TournamentResultsDAO
import zio.interop.catz.*
import zio.json.*

import java.util.UUID
import scala.util.control.NoStackTrace

enum TournamentsViewErrors(message: String) extends RuntimeException with NoStackTrace:
  case NoLatestTournament                  extends TournamentsViewErrors("Could not find latest tournament.")
  case NoTournamentFound(id: TournamentID) extends TournamentsViewErrors(s"No tournament with ${id} was found.")

object TournamentsView:
  import TournamentsViewErrors.*

  def lastTournaments(request: Request): ZIO[TransactorTask, Throwable, Response] = for
    tx     <- ZIO.service[TransactorTask]
    result <- sql"""SELECT version()""".query[String].to[List].transact(tx)
    out     = Response.text(s"PG version: ${result.mkString(", ")}")
  yield out

  def latestTournament(request: Request): RIO[TransactorTask, Response] = for
    maybeLatest <- TournamentResultsDAO.latest
    latest      <- ZIO.from(maybeLatest).orElseFail(NoLatestTournament)
  yield Response.json(latest.toJson)

  def read(id: TournamentID, request: Request): RIO[TransactorTask, Response] = for
    maybeTournament <- TournamentResultsDAO.read(id)
    tournament      <- ZIO.from(maybeTournament).orElseFail(NoTournamentFound(id))
  yield Response.text(tournament.toJson)
