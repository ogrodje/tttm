package si.ogrodje.tttm.v2.persistance

import doobie.*
import doobie.implicits.*
import si.ogrodje.tttm.v2.persistance.DB.TransactorTask
import si.ogrodje.tttm.v2.scoring.Scores
import si.ogrodje.tttm.v2.{MatchResult, PlayersConfig, TournamentID, TournamentResults}
import zio.*
import zio.interop.catz.*
import zio.json.*
import doobie.Fragment

import java.util.UUID

object TournamentResultsDAO:
  private type DAOTask[+Out] = RIO[TransactorTask, Out]

  given Put[UUID]               = Put[String].contramap(_.toString)
  given Put[List[MatchResult]]  = Put[String].contramap(_.toJson)
  given Put[PlayersConfig]      = Put[String].contramap(_.toJson)
  given Put[Scores]             = Put[String].contramap(_.toJson)
  given Read[TournamentResults] = Read[String].map(r =>
    TournamentResults.tournamentResultsDecoder
      .decodeJson(r)
      .left
      .map(err => new RuntimeException(s"Problem decoding with ${err}"))
      .toTry
      .get
  )
  private object queries:
    val insertResults: TournamentResults => Update0 =
      case TournamentResults(id, playersConfig, size3, size5, size7, size3scores, size5scores, size7scores) =>
        sql"""insert into tournaments (id, players_config,
              size_3, size_5, size_7,
              size_3_scores, size_5_scores, size_7_scores) VALUES (
             $id::uuid, $playersConfig::jsonb,
             $size3::jsonb, $size5::jsonb, $size7::jsonb,
             $size3scores::jsonb, $size5scores::jsonb, $size7scores::jsonb)"""
          .updateWithLabel("insert-tournament")

    private val orderByCreated    = Fragment.const("ORDER BY t.created_at DESC")
    private def limit(n: Int = 1) = sql" LIMIT $n"

    val queryResults =
      sql"""SELECT to_jsonb(t) as result
           |FROM tournaments t
           |""".stripMargin

    val findByID: TournamentID => Query0[TournamentResults] = id =>
      (queryResults ++ sql"WHERE t.id = ${id}::uuid" ++ limit(1))
        .queryWithLabel("find-tournament")

    val latest: Query0[TournamentResults] =
      (queryResults ++ orderByCreated ++ limit())
        .queryWithLabel("latest-tournament")

  def save(
    tournamentResults: TournamentResults
  ): DAOTask[TournamentID] =
    ZIO.serviceWithZIO[TransactorTask](queries.insertResults(tournamentResults).run.transact).as(tournamentResults.id)

  def read(
    id: TournamentID
  ): DAOTask[Option[TournamentResults]] =
    ZIO.serviceWithZIO[TransactorTask](queries.findByID(id).option.transact)

  def latest: DAOTask[Option[TournamentResults]] =
    ZIO.serviceWithZIO[TransactorTask](queries.latest.option.transact)
