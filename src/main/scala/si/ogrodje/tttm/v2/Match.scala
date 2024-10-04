package si.ogrodje.tttm.v2

import zio.ZIO.logInfo
import zio.http.{Client, URL}
import zio.json.*
import zio.stream.ZStream
import zio.{Scope, ZIO}

@jsonHintNames(SnakeCase)
final case class MatchResult(
  played: Long = 0,
  won: Long = 0,
  lost: Long = 0,
  tie: Long = 0,
  responseAverage: Double = -1,
  responseMedian: Double = -1,
  responseP99: Double = -1
) extends ServerMeasurements

object MatchResult:
  val empty: MatchResult                                 = apply()
  given matchResultJsonEncoder: JsonEncoder[MatchResult] = DeriveJsonEncoder.gen[MatchResult]

object Match:
  private type NumberOfGames = Long
  private type MatchResults  = Map[PlayerServerID, MatchResult]

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
    serverAUrl: URL,
    serverBUrl: URL,
    numberOfGames: NumberOfGames,
    concurrentProcesses: Int = 4,
    size: Size = Size.default,
    maybeGameplayReporter: Option[GameplayReporter] = None
  ): ZIO[Scope & Client, Throwable, Map[PlayerServerID, MatchResult]] =
    val servers @ (serverA, serverB) =
      ExternalPlayerServer.fromURL(serverAUrl) -> ExternalPlayerServer.fromURL(serverBUrl)
    val serverIDs @ (serverAID, serverBID) = (serverA.id, serverB.id)

    val aggregated: ZIO[
      Scope & Client,
      Throwable,
      (Map[PlayerServerID, MatchResult], Array[GameplayResult], (Array[Move], Array[Move]))
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
          Map[PlayerServerID, MatchResult](
            serverA.id -> MatchResult.empty,
            serverB.id -> MatchResult.empty
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

      resultsMap.map { case (playerServerID, matchResult: MatchResult) =>
        playerServerID -> matchResult.copy(
          responseAverage = -1,
          responseMedian = -1,
          responseP99 = -1
        )
      }
    }
