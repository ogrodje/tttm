package si.ogrodje.tttm.v2.persistance

import si.ogrodje.tttm.v2.persistance.DB.TransactorTask
import si.ogrodje.tttm.v2.{TournamentID, TournamentResults}
import zio.*
import doobie.*
import doobie.implicits.*
import zio.interop.catz.*

import java.util.UUID

object TournamentResultsDAO:
  def saveTournamentResults(
    tournamentResults: TournamentResults
  ): RIO[Scope & TransactorTask, TournamentID] =
    for
      id     <- ZIO.succeed(UUID.randomUUID())
      tx     <- ZIO.service[TransactorTask]
      result <- sql"""SELECT * FROM tournaments""".query[String].to[List].transact(tx)
      _      <- zio.Console.printLine(s"Save ${tournamentResults}")
    yield id
