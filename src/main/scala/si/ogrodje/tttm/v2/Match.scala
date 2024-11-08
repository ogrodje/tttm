package si.ogrodje.tttm.v2

import si.ogrodje.tttm.v2.Match.{MatchID, NumberOfGames}
import si.ogrodje.tttm.v2.Status.CrashedBy
import si.ogrodje.tttm.v2.persistance.GameplayResultQueue
import zio.*
import zio.ZIO.logInfo
import zio.http.Client
import zio.json.*
import zio.stream.ZStream

import java.util.UUID

@jsonHintNames(SnakeCase)
@jsonMemberNames(SnakeCase)
final case class MatchPlayerResult(
  played: Long = 0,
  won: Long = 0,
  lost: Long = 0,
  tie: Long = 0,
  crashed: Long = 0,
  @jsonField("response_average_ms") responseAverage: Double = 0,
  @jsonField("response_median_ms") responseMedian: Double = 0,
  @jsonField("response_p99_ms") responseP99: Double = 0,
  @jsonField("response_min_ms") responseMin: Double = 0,
  @jsonField("response_max_ms") responseMax: Double = 0,
  numberOfMoves: Int = 0,
  @jsonField("moves_per_game_average") movesPerGameAverage: Double = 0,
  @jsonField("game_ids") gameIDs: Set[GID] = Set.empty
) extends ServerMeasurements

object MatchPlayerResult:
  val empty: MatchPlayerResult                                 = apply()
  given matchResultJsonEncoder: JsonEncoder[MatchPlayerResult] = DeriveJsonEncoder.gen
  given matchResultJsonDecoder: JsonDecoder[MatchPlayerResult] = DeriveJsonDecoder.gen

@jsonHintNames(SnakeCase)
@jsonMemberNames(SnakeCase)
final case class MatchResult(
  @jsonField("player_x_id") playerXID: PlayerServerID,
  playerXResult: MatchPlayerResult = MatchPlayerResult.empty,
  @jsonField("player_o_id") playerOID: PlayerServerID,
  playerOResult: MatchPlayerResult = MatchPlayerResult.empty
)
object MatchResult:
  given matchResultJsonEncoder: JsonEncoder[MatchResult] = DeriveJsonEncoder.gen
  given matchResultJsonDecoder: JsonDecoder[MatchResult] = DeriveJsonDecoder.gen

  val empty: MatchResult = apply(playerXID = "x", playerOID = "o")

final case class Match private (
  id: MatchID,
  serverA: PlayerServer,
  serverB: PlayerServer,
  numberOfGames: NumberOfGames,
  size: Size = Size.default
):

  def playGames(
    concurrentProcesses: Int = 4,
    maybeGameplayResultQueue: Option[GameplayResultQueue] = None,
    maybeGameplayReporter: Option[GameplayReporter] = None,
    requestTimeout: Duration = Duration.fromSeconds(2)
  ): ZIO[Scope & Client, Throwable, MatchResult] =
    val serverIDS = (serverA.id, serverB.id)

    val matchStream =
      ZStream
        .range(0, numberOfGames.toInt, 1)
        .zipWithIndex
        .map { case (n, i) => (n, if i % 2 == 0 then (serverA, serverB) else (serverB, serverA)) }
        .mapZIOParUnordered(concurrentProcesses) { case (n, servers @ (localServerA, localServerB)) =>
          Gameplay
            .make(
              localServerA,
              localServerB,
              size,
              maybeGameplayReporter = maybeGameplayReporter,
              requestTimeout = requestTimeout
            )
            .play
            .map(result => servers -> result)
            .tap { case (servers, result @ (game, gameplayResult)) =>
              ZIO.succeed(maybeGameplayResultQueue).flatMap {
                case Some(q) => q.offer(result)
                case None    => ZIO.unit
              }
            }
            .tap { case (_, (game, gameplayResult)) =>
              // val gameplayResultJson = GameplayResult.gameplayResultJsonEncoder.encodeJson(gameplayResult, Some(2))
              logInfo(
                s"Completed game id: ${game.gid}, n: ${n + 1}; Size: ${game.size}, Moves: ${game.moves.length}, Status: ${game.status}"
              )
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

  private def mkMatchPlayerResultFrom(
    playerServerID: PlayerServerID,
    gameplayResults: List[GameplayResult]
  ): MatchPlayerResult =
    gameplayResults.foldLeft(MatchPlayerResult.empty) {
      case (
            matchPlayerResult @ MatchPlayerResult(
              played,
              won,
              lost,
              tie,
              crashed,
              responseAverage,
              responseMedian,
              responseP99,
              responseMin,
              responseMax,
              numberOfMoves,
              _,
              gameIDs
            ),
            gameplayResult @ GameplayResult(gid, duration, status, maybeWinner, serverA, serverB, moves)
          ) =>
        matchPlayerResult.copy(
          played = played + 1,
          won = maybeWinner
            .flatMap(s => Option.when(s == playerServerID)(won + 1))
            .getOrElse(won),
          lost = maybeWinner
            .flatMap(s => Option.when(s != playerServerID)(lost + 1))
            .getOrElse(lost),
          tie = maybeWinner.fold(tie + 1)(_ => tie),
          crashed = (maybeWinner, status) match
            case Some(winnerPlayerID) -> CrashedBy(_, _) if winnerPlayerID != playerServerID => crashed + 1
            case _                                                                           => crashed,
          numberOfMoves = numberOfMoves + moves.count(_.playerServerID.getOrElse("other") == playerServerID),
          gameIDs = gameIDs ++ Set(gid)
        )
    }

  private def attachStatsFromMoves(init: MatchPlayerResult = MatchPlayerResult.empty)(
    moves: List[Move]
  ): MatchPlayerResult =
    ServerMeasurements.fromMoves(moves.toArray)(
      { case (average, median, p99, min, max, numberOfMoves) =>
        init.copy(
          responseAverage = average,
          responseMedian = median,
          responseP99 = p99,
          responseMin = min,
          responseMax = max,
          movesPerGameAverage = init.numberOfMoves.toDouble / init.played.toDouble
        )
      },
      init
    )

object Match:
  private type NumberOfGames = Long
  private type MatchID       = UUID

  def mk(
    serverA: PlayerServer,
    serverB: PlayerServer,
    numberOfGames: NumberOfGames,
    size: Size = Size.default
  ): Match = apply(UUID.randomUUID(), serverA, serverB, numberOfGames, size)
