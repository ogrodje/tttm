package si.ogrodje.tttm.v2.scoring

import zio.{Task, ZIO}
import zio.stream.ZStream
import si.ogrodje.tttm.v2.{MatchPlayerResult, MatchResult, PlayerServerID, Size, Sizes, TournamentResults}
import eu.timepit.refined.auto.*

type Score            = Double
type Scores           = List[PlayerScore]
type TournamentScores = Map[Size, Scores]

object Scoring:
  private type MatchResults = List[MatchResult]
  private type Explain      = Boolean
  val defaultExplain: Explain = false

  extension (mr: MatchResults)
    private def filterFor(id: PlayerServerID): MatchResults =
      mr.filter(m => m.playerXID == id || m.playerOID == id)

  private def pickResults(tournamentResults: TournamentResults, size: Size): List[MatchResult] = size match
    case s if s == Size.unsafe(3) => tournamentResults.size3
    case s if s == Size.unsafe(5) => tournamentResults.size5
    case s if s == Size.unsafe(7) => tournamentResults.size7
    case _                        => Nil

  private def pickMatchPlayerResult(matchResult: MatchResult, id: PlayerServerID): MatchPlayerResult =
    if matchResult.playerXID == id then matchResult.playerXResult else matchResult.playerOResult

  private def pickOtherMatchPlayerResult(matchResult: MatchResult, id: PlayerServerID): MatchPlayerResult =
    if matchResult.playerXID != id then matchResult.playerXResult else matchResult.playerOResult

  final case class Step(op: Double => Double, explanation: String)
  private object Step:
    def empty: Step                                         = Step(identity, "0")
    def of(op: Double => Double, explanation: String): Step = apply(op, explanation)

  private def scoreMatchResults(id: PlayerServerID)(matchResult: MatchResult)(using explain: Explain): Score =
    val myMatchResults @ MatchPlayerResult(
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
      movesPerGameAverage,
      gameIDs
    ) =
      pickMatchPlayerResult(matchResult, id)

    val otherPlayerResults @ MatchPlayerResult(
      otherPlayed,
      otherWon,
      otherLost,
      otherTie,
      otherCrashed,
      otherResponseAverage,
      otherResponseMedian,
      otherResponseP99,
      otherResponseMin,
      otherResponseMax,
      otherNumberOfMoves,
      otherMovesPerGameAverage,
      otherGameIDs
    ) =
      pickOtherMatchPlayerResult(matchResult, id)

    if explain then println(s"""Inputs:
                               |Played = ${played}
                               |Won = ${won}
                               |Tie = ${tie}
                               |Lost = ${lost}
                               |Crashed = ${crashed}
                               |movesPerGameAverage = ${movesPerGameAverage}
                               |otherMovesPerGameAverage = ${otherMovesPerGameAverage}
                               |responseP99 = ${responseP99}
                               |otherResponseP99 = ${otherResponseP99}\n""".stripMargin)
    val sum = List(
      Step.empty,
      Step.of(_ => won.toDouble * 3, s"won * 3: $won * 3"),
      Step.of(_ => tie.toDouble * 1, s"tie * 1: $tie * 1"),
      Step.of(_ => lost.toDouble * 0, s"lost * 0: $lost * 0"),
      Step.of(
        // Less moves means 20% of won bonus points
        _ => if movesPerGameAverage < otherMovesPerGameAverage then (won.toDouble * 1.20).ceil else 0,
        s"Less moves bonus: IF ($movesPerGameAverage < $otherMovesPerGameAverage) THEN: ceil($won * 1.20)"
      ),
      Step.of(
        // If ties match and player is faster than number of played, 30% is bonus
        _ => if tie == otherTie && responseP99 < otherResponseP99 then (played.toDouble * 1.30).ceil else 0,
        s"Speed bonus: IF ($tie == $otherTie && $responseP99 < $otherResponseP99) THEN: ceil($played * 1.30)"
      ),
      Step.of(
        // Crash costs 1% of current score
        n => n - (1 to crashed.toInt).map(_ => n * 1.01).sum,
        s"Crash penalty: Crashed: $crashed"
      )
    ).zipWithIndex.foldLeft(0.toDouble) { case (agg, (step @ Step(op, explanation), i)) =>
      val result = op(agg)
      if explain then println(s"${i + 1}. $explanation = $result, Sum = ${agg + result}")
      agg + result
    }

    if explain then println("-" * 10 + s"\nSUM = $sum")
    sum

  def forSize(tournamentResults: TournamentResults, size: Size)(using explain: Explain = defaultExplain): Scores =
    val matchResults: MatchResults = pickResults(tournamentResults, size)
    tournamentResults.playersConfig.players
      .filter(_.sizes.contains(size))
      .map { player =>
        val playerID = player.id
        val sum      = matchResults.filterFor(playerID).map(scoreMatchResults(playerID)).sum
        if explain then println(s"Player ID: $playerID, Score = $sum")
        player.id -> sum
      }
      .map((id, score) => PlayerScore.apply(id, score))
      .sortBy(-_.score)

  def forTournament(tournamentResults: TournamentResults)(using
    explain: Explain = defaultExplain
  ): Task[TournamentScores] =
    val sizes: Sizes = Sizes.unsafe(
      List(
        if tournamentResults.size3.isEmpty then None else Some(Size.unsafe(3)),
        if tournamentResults.size5.isEmpty then None else Some(Size.unsafe(5)),
        if tournamentResults.size7.isEmpty then None else Some(Size.unsafe(7))
      ).collect { case Some(value) => value }
    )

    ZStream
      .fromIterable(sizes)
      .mapZIOParUnordered(3)(size => ZIO.succeed(Scoring.forSize(tournamentResults, size)).map(r => size -> r))
      .runCollect
      .map(_.toMap)
