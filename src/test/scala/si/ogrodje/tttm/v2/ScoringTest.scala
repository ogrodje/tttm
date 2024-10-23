package si.ogrodje.tttm.v2

import zio.*
import zio.logging.backend.SLF4J
import zio.test.*
import zio.json.*
import Status.*
import zio.test.TestAspect.*
import zio.http.*
import eu.timepit.refined.api.*
import java.net.MalformedURLException

object ScoringTest extends ZIOSpecDefault:
  override val bootstrap: ZLayer[Any, Any, zio.test.TestEnvironment] =
    Runtime.removeDefaultLoggers >>> SLF4J.slf4j >>> testEnvironment

  final case class FakePlayer private (endpoint: URL) extends PlayerServer:
    override def serverEndpoint: ServerEndpoint = endpoint
  object FakePlayer:
    def unsafeMake(endpoint: String): FakePlayer =
      URL.decode(endpoint).map(apply).toTry.get

  override def spec = suite("scoring") {
    test("test scoring") {
      val names @ List((n1, a1), (n2, a2), (n3, a3)) = List("P1", "P2", "P3").map(n =>
        RefType.applyRef[PlayerServerName](n).left.map(e => new RuntimeException(e)).toTry.get ->
          RefType.applyRef[AuthorName](n).left.map(e => new RuntimeException(e)).toTry.get
      )

      val players @ List(p1, p2, p3)   = names.map { case (serverName, authorName) =>
        Player(
          serverName,
          authorName,
          URL.decode(s"https://$serverName").toTry.get,
          URL.decode(s"https://$serverName").toTry.get,
          Sizes.unsafeOf(3)
        )
      }
      val ids @ List(p1ID, p2ID, p3ID) = players.map(_.id)

      val sampleResults = TournamentResults.empty
        .copy(
          playersConfig = PlayersConfig.empty.copy(
            players = players
          ),
          size3 = MatchResult.empty.copy(
            playerXID = p1ID,
            playerOID = p2ID,
            playerXResult = MatchPlayerResult.empty.copy(
              played = 10,
              won = 6,
              crashed = 1,
              lost = 1,
              tie = 3,
              movesPerGameAverage = 4
            ),
            playerOResult = MatchPlayerResult.empty.copy(
              played = 10,
              won = 3,
              crashed = 0,
              lost = 7,
              movesPerGameAverage = 6
            )
          ) :: MatchResult.empty.copy(
            playerXID = p2ID,
            playerOID = p3ID,
            playerXResult = MatchPlayerResult.empty.copy(
              played = 10,
              won = 10,
              movesPerGameAverage = 6
            ),
            playerOResult = MatchPlayerResult.empty.copy(
              played = 10,
              lost = 10,
              movesPerGameAverage = 5
            )
          ) :: MatchResult.empty.copy(
            playerXID = p1ID,
            playerOID = p3ID,
            playerXResult = MatchPlayerResult.empty.copy(
              played = 10,
              won = 4,
              lost = 1,
              crashed = 1,
              tie = 5,
              movesPerGameAverage = 6,
              responseP99 = 40
            ),
            playerOResult = MatchPlayerResult.empty.copy(
              played = 10,
              lost = 6,
              crashed = 1,
              tie = 5,
              movesPerGameAverage = 7,
              responseP99 = 50
            )
          ) :: Nil,
          size5 = Nil,
          size7 = Nil
        )

      // println(TournamentResults.tournamentResultsEncoder.encodeJson(sampleResults, Some(1)))
      val scoringResult = Scoring.forSize(sampleResults, Size.unsafe(3))(using explain = false)
      println(scoringResult)
      assertTrue(1 == 1)
    }
  }
