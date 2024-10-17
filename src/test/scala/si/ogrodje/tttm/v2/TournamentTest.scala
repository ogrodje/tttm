package si.ogrodje.tttm.v2

import eu.timepit.refined.api.RefType
import zio.*
import zio.http.{Client, URL}
import zio.logging.backend.SLF4J
import zio.test.*
import eu.timepit.refined.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.api.*
import eu.timepit.refined.predicates.all.NonEmpty
import eu.timepit.refined.generic.*
import eu.timepit.refined.string.*

import java.net.URI
import java.util.UUID

object TournamentTest extends ZIOSpecDefault:
  override val bootstrap: ZLayer[Any, Any, zio.test.TestEnvironment] =
    Runtime.removeDefaultLoggers >>> SLF4J.slf4j >>> testEnvironment

  private val authorName                         =
    RefType.applyRef[AuthorName]("Oto Brglez").left.map(v => new RuntimeException(s"Boom ${v}")).toTry.get
  private val nameOf: String => PlayerServerName = rawName =>
    RefType.applyRef[PlayerServerName](rawName).left.map(v => new RuntimeException(s"Boom ${v}")).toTry.get

  private val authorURL = URL.decode("https://epic.blog").toTry.get

  private val p1 = Player(
    nameOf("p1"),
    authorName,
    authorURL,
    endpointURL = URL.fromURI(URI.create("https://serverA")).get,
    sizes = Sizes.validSizes,
    tags = Set("demo", "test", "fake")
  )

  private val config: PlayersConfig = PlayersConfig.fromPlayers(
    p1,
    p1.copy(
      name = nameOf("p2"),
      endpointURL = URL.decode("https://serverB").toTry.get,
      sizes = Sizes.unsafeOf(3, 5)
    ),
    p1.copy(
      name = nameOf("p3"),
      endpointURL = URL.decode("https://serverC").toTry.get,
      sizes = Sizes.unsafeOf(5, 7)
    ),
    p1.copy(name = nameOf("p4"), endpointURL = URL.decode("https://serverD").toTry.get, sizes = Sizes.unsafeOf(7)),
    p1.copy(name = nameOf("p5"), endpointURL = URL.decode("https://serverD").toTry.get, sizes = Sizes.unsafeOf(7))
  )

  def spec = suite("Tournament")(
    test("matches generator") {
      val verbose    = !true
      val tournament = Tournament.fromPlayersConfig(config)

      tournament.tournamentMatches.foreach { case (size, m) =>
        if verbose then println(s"Size: ${size}")
        m.foreach(m => if verbose then println(s"\t- M: ${m.serverA.id} VS ${m.serverB.id}"))
      }

      assertTrue(
        tournament.tournamentMatches(Size.unsafe(3)).length == 1,
        tournament.tournamentMatches(Size.unsafe(5)).length == 3,
        tournament.tournamentMatches(Size.unsafe(7)).length == 6
      )
    },
    test("play the tournament") {
      val verbose    = !true
      val tournament = Tournament.fromPlayersConfig(config)
      val pom        =
        tournament
          .play(requestTimeout = Duration.fromMillis(100L))
          .provideLayer(Client.default.and(Scope.default))
          .runDrain

      assertZIO(pom)(
        Assertion.assertion("test") { results =>
          println(s"got => ${results}")
          true == true
        }
      )
    }
  )
