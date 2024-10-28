package si.ogrodje.tttm.v2.server

import si.ogrodje.tttm.v2.persistance.DB.TransactorTask
import si.ogrodje.tttm.v2.persistance.TournamentResultsDAO
import si.ogrodje.tttm.v2.scoring.Score
import si.ogrodje.tttm.v2.*
import zio.http.*
import zio.json.*
import zio.{RIO, ZIO}

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, ZoneId}
import scala.util.control.NoStackTrace

enum TournamentsViewErrors(message: String) extends RuntimeException with NoStackTrace:
  case NoLatestTournament                  extends TournamentsViewErrors("Could not find latest tournament.")
  case NoTournamentFound(id: TournamentID) extends TournamentsViewErrors(s"No tournament with ${id} was found.")

object TournamentsView:
  import TournamentsViewErrors.*

  def latestTournament(request: Request): RIO[TransactorTask, Response] = for
    maybeLatest <- TournamentResultsDAO.latest
    latest      <- ZIO.from(maybeLatest).orElseFail(NoLatestTournament)
  yield Response.json(latest.toJson)

  def read(id: TournamentID, request: Request): RIO[TransactorTask, Response] = for
    maybeTournament <- TournamentResultsDAO.read(id)
    tournament      <- ZIO.from(maybeTournament).orElseFail(NoTournamentFound(id))
  yield Response.text(tournament.toJson)

  def rankingFor(size: Size): RIO[TransactorTask, Response] =
    for rankings <- TournamentResultsDAO.rankingFor(size)
    yield Response.json(rankings.toJson)

  private val dbFormat: DateTimeFormatter              = DateTimeFormatter.ofPattern("yyyy-MM-dd").withZone(ZoneId.systemDefault())
  private def unsafeDateParser(raw: String): LocalDate = LocalDate.parse(raw, dbFormat)

  def rankingForWithPlayers(size: Size): RIO[PlayersConfig & TransactorTask, Response] = for
    players           <-
      ZIO.serviceWith[PlayersConfig](_.players.filter(_.sizes.contains(size)))
    rankings          <- TournamentResultsDAO.rankingFor(size)
    results            =
      rankings
        .groupBy(_.playerServerID)
        .map((id, rankings) =>
          id -> rankings
            .map(row => (row.tournamentID, unsafeDateParser(row.day), row.ranking, row.score))
            .sortBy(_._1)
            .reverse
        )
        .toList
        .sortBy(_._2.headOption.map(_._2))
    rankingWithPlayers =
      results.flatMap { case (serverID, results) =>
        players
          .find(player => player.id == serverID)
          .map(player =>
            RankingWithPlayer(
              serverID,
              player,
              results,
              rank = results.headOption.map(_._3).getOrElse(-1)
            )
          )
      }.sortBy(_.rank)
  yield Response.json(rankingWithPlayers.toJson)

@jsonMemberNames(SnakeCase)
final case class RankingWithPlayer(
  @jsonField("player_server_id") serverID: PlayerServerID,
  player: Player,
  scores: List[(String, LocalDate, Int, Score)],
  rank: Int
)
object RankingWithPlayer:
  given rankingWithPlayerEncoder: JsonEncoder[RankingWithPlayer] = DeriveJsonEncoder.gen
