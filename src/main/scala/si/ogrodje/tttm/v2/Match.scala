package si.ogrodje.tttm.v2

import zio.ZIO.logInfo
import zio.http.{Client, URL}
import zio.json.*
import zio.stream.ZStream
import zio.{Scope, ZIO}

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
  @jsonField("player_x_result") playerXResult: MatchPlayerResult = MatchPlayerResult.empty,
  @jsonField("player_o_result") playerOResult: MatchPlayerResult = MatchPlayerResult.empty
)
object MatchResult:
  given matchResultJsonEncoder: JsonEncoder[MatchResult] = DeriveJsonEncoder.gen[MatchResult]
  val empty: MatchResult                                 = apply()

object Match:
  private type NumberOfGames = Long
  private type MatchResults  = Map[PlayerServerID, MatchPlayerResult]

  private def updateResults(
    acc: MatchResults,
    serverA: PlayerServerID,
    serverB: PlayerServerID,
    gameplayResult: GameplayResult
  ): MatchResults =
    val maybeWinner          = gameplayResult.maybeWinner
    val (resultsA, resultsB) = acc(serverA) -> acc(serverB)

    acc ++ Map(
      serverA -> resultsA.copy(
        played = resultsA.played + 1,
        won = maybeWinner
          .flatMap(s => Option.when(s == serverA)(resultsA.won + 1))
          .getOrElse(resultsA.won),
        lost = maybeWinner
          .flatMap(s => Option.when(s == serverB)(resultsA.lost + 1))
          .getOrElse(resultsA.lost),
        tie = maybeWinner.fold(resultsA.tie + 1)(_ => resultsA.tie)
      ),
      serverB -> resultsB.copy(
        played = resultsB.played + 1,
        won = maybeWinner
          .flatMap(s => Option.when(s == serverB)(resultsB.won + 1))
          .getOrElse(resultsB.won),
        lost = maybeWinner
          .flatMap(s => Option.when(s == serverA)(resultsB.lost + 1))
          .getOrElse(resultsB.lost),
        tie = maybeWinner.fold(resultsB.tie + 1)(_ => resultsB.tie)
      )
    )

  def playGames(
    serverA: PlayerServer,
    serverB: PlayerServer,
    numberOfGames: NumberOfGames,
    concurrentProcesses: Int = 4,
    size: Size = Size.default,
    maybeGameplayReporter: Option[GameplayReporter] = None
  ) =
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
              logInfo(s"Completed game n: ${n}; Moves: ${g.moves.length}, Status: ${g.status}")
            }
        }

    val x = matchStream.runFold(
      (
        (serverA.id, List.empty[GameplayResult], List.empty[Move]),
        (serverB.id, List.empty[GameplayResult], List.empty[Move])
      )
    ) { case (agg, ((b1, b2), (game, gameplayResult))) =>
      println(s"b1 => ${b1}, b2 => ${b2}")
      (
        (
          serverA.id,
          agg._1._2 ++ Seq(gameplayResult),
          agg._1._3 ++ gameplayResult.moves.filter {
            case Move(_, _, _, Some(id)) if id == serverA.id => true
            case _                                           => false
          }
        ),
        (
          serverB.id,
          (agg._2._2 ++ Seq(gameplayResult)),
          agg._2._3 ++ gameplayResult.moves.filter {
            case Move(_, _, _, Some(id)) if id == serverB.id => true
            case _                                           => false
          }
        )
      )
    }

    x.map { case ((p1, gr1, m1), (p2, gr2, m2)) =>
      MatchResult(
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
    val measurements: Array[Double] = moves.map(_.duration.toMillis.toDouble).toArray
    if measurements.isEmpty then throw new IllegalArgumentException("Moves array cannot be empty")
    val average                     = measurements.sum / measurements.length.toDouble

    // Sort array for median and p99 calculations
    val sorted = measurements.sorted

    // Median calculation
    val median = if sorted.length % 2 == 0 then
      val mid = sorted.length / 2
      (sorted(mid - 1) + sorted(mid)) / 2
    else sorted(sorted.length / 2)

    // P99 calculation
    val p99Index = math.ceil(measurements.length * 0.99).toInt - 1
    val p99      = sorted(p99Index)

    init.copy(
      responseAverage = average,
      responseMedian = median,
      responseP99 = p99,
      responseMin = measurements.min,
      responseMax = measurements.max
    )

  def playGamesX(
    serverAUrl: URL,
    serverBUrl: URL,
    numberOfGames: NumberOfGames,
    concurrentProcesses: Int = 4,
    size: Size = Size.default,
    maybeGameplayReporter: Option[GameplayReporter] = None
  ): ZIO[Scope & Client, Throwable, Map[PlayerServerID, MatchPlayerResult]] =
    val servers @ (serverA, serverB) =
      ExternalPlayerServer.fromURL(serverAUrl) -> ExternalPlayerServer.fromURL(serverBUrl)
    val serverIDs @ (serverAID, serverBID) = (serverA.id, serverB.id)

    val aggregated: ZIO[
      Scope & Client,
      Throwable,
      (Map[PlayerServerID, MatchPlayerResult], Array[GameplayResult], (Array[Move], Array[Move]))
    ] = ZStream
      .range(0, numberOfGames.toInt)
      .zipWithIndex
      .map { case (n, i) => (n, if i % 2 == 0 then (serverA, serverB) else (serverB, serverA)) }
      .mapZIOParUnordered(concurrentProcesses) { case (n, (serverA, serverB)) =>
        Gameplay
          .make(serverA, serverB, size, maybeGameplayReporter = maybeGameplayReporter)
          .play
          .map(result => (serverA, serverB) -> result)
          .tap(r => logInfo(s"Completed game n. ${n}; moves: ${r._2._1.moves.length}"))
      }
      .runFold(
        (
          Map[PlayerServerID, MatchPlayerResult](
            serverA.id -> MatchPlayerResult.empty,
            serverB.id -> MatchPlayerResult.empty
          ),
          Array.empty[GameplayResult],
          (Array.empty[Move], Array.empty[Move])
        )
      ) {
        case (
              (acc, gameplayResults, (movesA, movesB)),
              ((localServerA, localServerB), (game: Game, gameplayResult: GameplayResult))
            ) =>
          val newMovesA =
            if serverAID == localServerA.id then game.moves.filter(_.symbol == X)
            else game.moves.filter(_.symbol == O)

          val newMovesB =
            if serverBID == localServerB.id then game.moves.filter(_.symbol == O)
            else game.moves.filter(_.symbol == X)

          (
            updateResults(acc, localServerA.id, localServerB.id, gameplayResult),
            gameplayResults ++ Seq(gameplayResult),
            (movesA ++ newMovesA, movesB ++ newMovesB)
          )
      }

    aggregated.map { case (resultsMap, gameplayResults, (movesA, movesB)) =>
      println(gameplayResults.map(_.toJson).mkString("\n"))

      // val serverAMeasurements = gameplayResults.map(_.serverA)
      // val serverBMeasurements = gameplayResults.map(_.serverB)

      // println(s"Moves A => ${movesA.length}")
      // println(s"Moves B => ${movesB.length}")

      resultsMap.map { case (playerServerID, matchResult: MatchPlayerResult) =>
        playerServerID -> matchResult.copy(
          responseAverage = -1,
          responseMedian = -1,
          responseP99 = -1
        )
      }
    }
