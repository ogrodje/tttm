package si.ogrodje.tttm.v2

import java.time.{LocalDateTime, ZoneId}
import java.util.UUID
import zio.{Duration, Scope, ZIO}
import zio.http.Client
import zio.stream.{Stream, ZStream}

final case class Tournament private (
  id: Tournament.TournamentID,
  createdAt: LocalDateTime,
  tournamentMatches: Tournament.TournamentMatches = Map.empty
):
  private val zone: ZoneId = ZoneId.systemDefault()

  def play(
    concurrentPlayProcesses: Int = 3,
    concurrentGameProcesses: Int = 1,
    maybeGameplayReporter: Option[GameplayReporter] = None,
    requestTimeout: Duration = Duration.fromSeconds(2)
  ): ZStream[Scope & Client, Throwable, MatchResult] =
    ZStream
      .fromIterator(tournamentMatches.keysIterator)
      .flatMap(size => ZStream.fromIterable(tournamentMatches(size)))
      .mapZIOParUnordered(concurrentPlayProcesses) { gMatch =>
        gMatch
          .playGames(
            concurrentProcesses = concurrentGameProcesses,
            requestTimeout = requestTimeout
          )
          .tap(matchResult =>
            zio.Console.printLine(
              "--- TODO: match results here ---"
              // MatchResult.matchResultJsonEncoder.encodeJson(matchResult, Some(2))
              // s"MR => ${mr}"
            )
          )
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
    numberOfGames: Long = 10
  ): Tournament =
    apply(
      id = UUID.randomUUID(),
      createdAt = LocalDateTime.now(zone),
      tournamentMatches = Sizes.validSizes.map { size =>
        val pairs = mkPairs(playersConfig.players.filter(_.sizes.contains(size)))
        size -> pairs.map { case (a, b) => Match.mk(a, b, numberOfGames, size) }
      }.toMap
    )
