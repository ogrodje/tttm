package si.ogrodje.tttm.v2.persistance

import doobie.*
import doobie.implicits.*
import si.ogrodje.tttm.v2.persistance.DB.TransactorTask
import si.ogrodje.tttm.v2.{MatchResult, PlayersConfig, TournamentID, TournamentResults}
import zio.*
import zio.interop.catz.*
import zio.json.*

import java.util.UUID

object TournamentResultsDAO:
  given Put[UUID]              = Put[String].contramap(_.toString)
  given Put[List[MatchResult]] = Put[String].contramap(_.toJson)
  given Put[PlayersConfig]     = Put[String].contramap(_.toJson)

  private object queries:
    val insertResults: TournamentResults => Update0 = {
      case TournamentResults(id, playersConfig, size3, size5, size7) =>
        sql"""insert into tournaments (id, players_config, size_3, size_5, size_7) VALUES (
             $id::uuid, $playersConfig::jsonb, $size3::jsonb, $size5::jsonb, $size7::jsonb)"""
          .updateWithLabel("insert-tournament")
    }

  def saveTournamentResults(
    tournamentResults: TournamentResults
  ): RIO[Scope & TransactorTask, TournamentID] =
    ZIO.serviceWithZIO[TransactorTask](queries.insertResults(tournamentResults).run.transact).as(tournamentResults.id)
