package si.ogrodje.tttm.v2

import si.ogrodje.tttm.v2.Match.NumberOfGames
import zio.*
import zio.ZIO.logInfo
import zio.http.Client
import zio.json.*
import zio.stream.ZStream

@jsonHintNames(SnakeCase)
final case class MatchPlayerResult(
  played: Long = 0,
  won: Long = 0,
  lost: Long = 0,
  tie: Long = 0,
  @jsonField("response_average_ms") responseAverage: Double = -1,
  @jsonField("response_median_ms") responseMedian: Double = -1,
  @jsonField("response_p99_ms") responseP99: Double = -1,
  @jsonField("response_min_ms") responseMin: Double = -1,
  @jsonField("response_max_ms") responseMax: Double = -1
) extends ServerMeasurements

object MatchPlayerResult:
  val empty: MatchPlayerResult                                 = apply()
  given matchResultJsonEncoder: JsonEncoder[MatchPlayerResult] = DeriveJsonEncoder.gen[MatchPlayerResult]

@jsonHintNames(SnakeCase)
final case class MatchResult(
  @jsonField("player_x_id") playerXID: PlayerServerID,
  @jsonField("player_x_result") playerXResult: MatchPlayerResult = MatchPlayerResult.empty,
  @jsonField("player_o_id") playerOID: PlayerServerID,
  @jsonField("player_o_result") playerOResult: MatchPlayerResult = MatchPlayerResult.empty
)
object MatchResult:
  given matchResultJsonEncoder: JsonEncoder[MatchResult] = DeriveJsonEncoder.gen[MatchResult]
  val empty: MatchResult                                 = apply(playerXID = "x", playerOID = "o")

final case class Match private (
  serverA: PlayerServer,
  serverB: PlayerServer,
  numberOfGames: NumberOfGames,
  size: Size = Size.default,
  maybeGameplayReporter: Option[GameplayReporter] = None
):

  def playGames(
    concurrentProcesses: Int = 4
  ): ZIO[Scope & Client, Throwable, MatchResult] =
    val serverIDS = (serverA.id, serverB.id)

    val matchStream =
      ZStream
        .range(0, numberOfGames.toInt, 1)
        .zipWithIndex
        .map { case (n, i) => (n, if i % 2 == 0 then (serverA, serverB) else (serverB, serverA)) }
        .mapZIOParUnordered(concurrentProcesses) { case (n, servers @ (localServerA, localServerB)) =>
          Gameplay
            .make(localServerA, localServerB, size, maybeGameplayReporter = maybeGameplayReporter)
            .play
            .map(result => servers -> result)
            .tap { case (_, (g, result)) =>
              logInfo(s"Completed game n: $n; Moves: ${g.moves.length}, Status: ${g.status}")
            }
        }

    val x = matchStream.runFold(
      (
        (serverA.id, List.empty[GameplayResult], List.empty[Move]),
        (serverB.id, List.empty[GameplayResult], List.empty[Move])
      )
    ) { case (agg @ ((pA, grA, movesA), (pB, grB, movesB)), ((b1, b2), (game, gameplayResult))) =>
      (
        (
          serverA.id,
          grA ++ Seq(gameplayResult),
          movesA ++ gameplayResult.moves.filter {
            case Move(_, _, _, Some(id)) if id == pA => true
            case _                                   => false
          }
        ),
        (
          serverB.id,
          grB ++ Seq(gameplayResult),
          movesB ++ gameplayResult.moves.filter {
            case Move(_, _, _, Some(id)) if id == pB => true
            case _                                   => false
          }
        )
      )
    }

    x.map { case ((p1, gr1, m1), (p2, gr2, m2)) =>
      MatchResult(
        playerXID = p1,
        playerOID = p2,
        playerXResult = attachStatsFromMoves(mkMatchPlayerResultFrom(p1, gr1))(m1),
        playerOResult = attachStatsFromMoves(mkMatchPlayerResultFrom(p2, gr2))(m2)
      )
    }

  private def mkMatchPlayerResultFrom(playerServerID: PlayerServerID, gr: List[GameplayResult]): MatchPlayerResult =
    gr.foldLeft(MatchPlayerResult.empty) { case (agg, c) =>
      agg.copy(
        played = agg.played + 1,
        won = c.maybeWinner.flatMap(s => Option.when(s == playerServerID)(agg.won + 1)).getOrElse(agg.won),
        lost = c.maybeWinner.flatMap(s => Option.when(s != playerServerID)(agg.lost + 1)).getOrElse(agg.lost),
        tie = c.maybeWinner.fold(agg.tie + 1)(_ => agg.tie)
      )
    }

  private def attachStatsFromMoves(init: MatchPlayerResult = MatchPlayerResult.empty)(
    moves: List[Move]
  ): MatchPlayerResult =
    ServerMeasurements.fromMoves(moves) { case (average, median, p99, min, max) =>
      init.copy(
        responseAverage = average,
        responseMedian = median,
        responseP99 = p99,
        responseMin = min,
        responseMax = max
      )
    }

object Match:
  private type NumberOfGames = Long

  def mk(
    serverA: PlayerServer,
    serverB: PlayerServer,
    numberOfGames: NumberOfGames,
    size: Size = Size.default,
    maybeGameplayReporter: Option[GameplayReporter] = None
  ): Match = apply(serverA, serverB, numberOfGames, size, maybeGameplayReporter)
