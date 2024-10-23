package si.ogrodje.tttm.v2
import zio.json.*

object Scoring:
  type MatchResults = List[MatchResult]
  type Explain      = Boolean
  val defaultExplain: Explain = true

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

  final case class Step[-In](op: In => Double, explanation: String)
  private object Step:
    def empty: Step[Double] = Step((n: Double) => 0, "0")

  private def scoreMatchResults(matchResult: MatchResult, id: PlayerServerID)(using explain: Explain): Double =
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
      movesPerGameAverage
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
      otherMovesPerGameAverage
    ) =
      pickOtherMatchPlayerResult(matchResult, id)

    val sum = List(
      Step.empty,
      Step[Double](n => won.toDouble * 3, s"won * 3: $won * 3"),
      Step[Double](n => tie.toDouble * 1, s"tie * 1: $tie * 1"),
      Step[Double](n => lost.toDouble * 0, s"lost * 0: $lost * 0"),
      Step[Double](
        // Less moves means 20% of won bonus points
        _ => if movesPerGameAverage < otherMovesPerGameAverage then (won.toDouble * 1.20).ceil else 0,
        s"Less moves bonus: IF ($movesPerGameAverage < $otherMovesPerGameAverage) THEN: ceil($won * 1.20)"
      ),
      Step[Double](
        // If ties match and player is faster than number of played, 30% is bonus
        _ => if tie == otherTie && responseP99 < otherResponseP99 then (played.toDouble * 1.30).ceil else 0,
        s"Speed bonus: IF ($tie == $otherTie && $responseP99 < $otherResponseP99) THEN: ceil($played * 1.30)"
      ),
      Step[Double](
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

  type Scores = Map[PlayerServerID, Double]
  def forSize(tournamentResults: TournamentResults, size: Size)(using explain: Explain = defaultExplain): Scores =
    val matchResults: MatchResults = pickResults(tournamentResults, size)
    tournamentResults.playersConfig.players
      .filter(_.sizes.contains(size))
      .map { player =>
        val playerID = player.id
        val sum      = matchResults.filterFor(playerID).map(mr => scoreMatchResults(mr, playerID)).sum
        if explain then println(s"Player ID: ${playerID}, Score = ${sum}")
        player.id -> sum
      }
      .sortBy(-_._2)
      .toMap
