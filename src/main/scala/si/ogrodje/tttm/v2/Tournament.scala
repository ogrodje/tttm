package si.ogrodje.tttm.v2

import zio.http.Client
import zio.json.*
import zio.stream.ZStream
import zio.{Duration, RIO, Scope}

import java.time.{LocalDateTime, ZoneId}
import java.util.UUID

@jsonMemberNames(SnakeCase)
final case class TournamentResults(
  playersConfig: PlayersConfig = PlayersConfig.empty,
  @jsonField("size_3") size3: List[MatchResult],
  @jsonField("size_5") size5: List[MatchResult],
  @jsonField("size_7") size7: List[MatchResult]
)
object TournamentResults:
  given tournamentResultsEncoder: JsonEncoder[TournamentResults] = DeriveJsonEncoder.gen
  val empty: TournamentResults                                   = apply(
    PlayersConfig.empty,
    List.empty,
    List.empty,
    List.empty
  )

final case class Tournament private (
  id: Tournament.TournamentID,
  createdAt: LocalDateTime,
  playersConfig: PlayersConfig,
  tournamentMatches: Tournament.TournamentMatches = Map.empty
):
  private val zone: ZoneId = ZoneId.systemDefault()

  given Ordering[MatchResult] = Ordering.by { (mr: MatchResult) =>
    (mr.playerXResult.won, mr.playerOResult.won)
  }

  def play(
    concurrentPlayProcesses: Int = 3,
    concurrentGameProcesses: Int = 1,
    maybeGameplayReporter: Option[GameplayReporter] = None,
    requestTimeout: Duration = Duration.fromSeconds(2)
  ): RIO[Scope & Client, TournamentResults] =
    val (s3, s5, s7) = (Size.unsafe(3), Size.unsafe(5), Size.unsafe(7))
    playStream(
      concurrentPlayProcesses,
      concurrentGameProcesses,
      maybeGameplayReporter,
      requestTimeout
    ).runFold(TournamentResults.empty) { case (tournamentResults, (gameMatch, matchResult)) =>
      gameMatch.size match
        case `s3` => tournamentResults.copy(size3 = (tournamentResults.size3 ++ List(matchResult)).sorted)
        case `s5` => tournamentResults.copy(size5 = (tournamentResults.size5 ++ List(matchResult)).sorted)
        case `s7` => tournamentResults.copy(size7 = (tournamentResults.size7 ++ List(matchResult)).sorted)
    }.map(_.copy(playersConfig = playersConfig))

  private def playStream(
    concurrentPlayProcesses: Int = 3,
    concurrentGameProcesses: Int = 1,
    maybeGameplayReporter: Option[GameplayReporter] = None,
    requestTimeout: Duration = Duration.fromSeconds(2)
  ): ZStream[Scope & Client, Throwable, (Match, MatchResult)] =
    ZStream
      .fromIterator(tournamentMatches.keysIterator)
      .flatMap(size => ZStream.fromIterable(tournamentMatches(size)))
      .mapZIOParUnordered(concurrentPlayProcesses) { gMatch =>
        gMatch
          .playGames(
            concurrentProcesses = concurrentGameProcesses,
            requestTimeout = requestTimeout
          )
          .map(gMatch -> _)
      }

object Tournament:
  private type TournamentID = UUID
  type NumberOfGames        = Long
  val numberOfGames: NumberOfGames = 10 // Number of games in match.
  private type TournamentMatches = Map[Size, List[Match]]
  private val zone: ZoneId = ZoneId.systemDefault()

  private def mkPairs(players: List[Player]): List[(Player, Player)] = for
    (player1, index1) <- players.zipWithIndex
    (player2, index2) <- players.zipWithIndex
    if index1 < index2
  yield player1 -> player2

  def fromPlayersConfig(
    playersConfig: PlayersConfig,
    sizes: Sizes = Sizes.validSizes,
    numberOfGames: Long = 10
  ): Tournament = apply(
    id = UUID.randomUUID(),
    createdAt = LocalDateTime.now(zone),
    playersConfig = playersConfig,
    tournamentMatches = sizes.map { size =>
      val pairs = mkPairs(playersConfig.players.filter(_.sizes.contains(size)))
      size -> pairs.map { case (a, b) => Match.mk(a, b, numberOfGames, size) }
    }.toMap
  )
