package si.ogrodje.tttm.v2.persistance

import doobie.*
import doobie.implicits.*
import si.ogrodje.tttm.v2.persistance.DB.TransactorTask
import si.ogrodje.tttm.v2.scoring.{Score, Scores}
import si.ogrodje.tttm.v2.{MatchResult, PlayerServerID, PlayersConfig, Size, TournamentID, TournamentResults}
import zio.*
import zio.interop.catz.*
import zio.json.*
import doobie.Fragment

import java.util.UUID

@jsonMemberNames(SnakeCase)
final case class RankingRow(
  tournamentID: String,
  day: String,
  @jsonField("player_server_id") playerServerID: PlayerServerID,
  score: Score,
  ranking: Int
)
object RankingRow:
  given rankingRowEncoder: JsonEncoder[RankingRow] = DeriveJsonEncoder.gen

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

    private val queryResults: Fragment =
      sql"""SELECT to_jsonb(t) as result
           |FROM tournaments t
           |""".stripMargin

    val findByID: TournamentID => Query0[TournamentResults] = id =>
      (queryResults ++ sql"WHERE t.id = $id::uuid" ++ limit(n = 1))
        .queryWithLabel("find-tournament")

    val latest: Query0[TournamentResults] =
      (queryResults ++ orderByCreated ++ limit())
        .queryWithLabel("latest-tournament")

    def rankingForRange(size: Size, interval: String = "30 days"): Query0[RankingRow] =
      val sizeTable: Fragment = size match
        case s if s == Size.unsafe(3) => Fragment.const("t.size_3_scores")
        case s if s == Size.unsafe(5) => Fragment.const("t.size_5_scores")
        case s if s == Size.unsafe(7) => Fragment.const("t.size_7_scores")

      (sql"""SELECT
                tournament_id,
                day,
                (day_scores ->> 'id')   as server_id,
                (day_scores -> 'score') as score,
                row_number() OVER (PARTITION BY day ORDER BY
                    day DESC,
                    (day_scores -> 'score') DESC
                    )                   AS day_ranking
           FROM (SELECT t.created_at::timestamp::date       as day,
                        jsonb_array_elements(""" ++ sizeTable ++ sql""") as day_scores,
                        t.id as tournament_id
                 FROM tournaments t
                 WHERE
                    t.created_at::timestamp::date >= CURRENT_DATE - ${interval}::interval
                 GROUP BY
                    t.created_at::timestamp::date,
                    """ ++ sizeTable ++ sql"""
                    , t.id
                 ORDER BY
                    t.created_at::timestamp::date DESC,
                    """ ++ sizeTable ++ sql""" -> 'score' DESC) as day_scores;
           """.stripMargin).queryWithLabel("last-ranking")

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

  def rankingFor(size: Size): DAOTask[List[RankingRow]] =
    ZIO.serviceWithZIO[TransactorTask](queries.rankingForRange(size).to[List].transact)
