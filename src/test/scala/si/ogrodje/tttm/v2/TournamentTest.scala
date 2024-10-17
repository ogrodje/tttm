package si.ogrodje.tttm.v2

import eu.timepit.refined.api.RefType
import zio.*
import zio.http.*
import zio.http.netty.NettyConfig
import zio.http.netty.server.NettyDriver
import zio.logging.backend.SLF4J
import zio.test.*

import java.net.URI

object TournamentTest extends ZIOSpecDefault:
  override val bootstrap: ZLayer[Any, Any, zio.test.TestEnvironment] =
    Runtime.removeDefaultLoggers >>> SLF4J.slf4j >>> testEnvironment

  private val authorName                         =
    RefType.applyRef[AuthorName]("Oto Brglez").left.map(v => new RuntimeException(s"Boom $v")).toTry.get
  private val nameOf: String => PlayerServerName = rawName =>
    RefType.applyRef[PlayerServerName](rawName).left.map(v => new RuntimeException(s"Boom $v")).toTry.get

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
        if verbose then println(s"Size: $size")
        m.foreach(m => if verbose then println(s"\t- M: ${m.serverA.id} VS ${m.serverB.id}"))
      }

      assertTrue(
        tournament.tournamentMatches(Size.unsafe(3)).length == 1,
        tournament.tournamentMatches(Size.unsafe(5)).length == 3,
        tournament.tournamentMatches(Size.unsafe(7)).length == 6
      )
    },
    test("play the tournament") {
      val verbose = !true

      val x = (for
        port              <- ZIO.serviceWithZIO[Server](_.port)
        tournament         =
          Tournament.fromPlayersConfig(config.copy(players = config.players.zipWithIndex.map { case (player, i) =>
            player.copy(
              endpointURL = URL.root.port(port).path(s"/${player.id}")
              // sizes = Sizes.unsafeOf(3)
            )
          }))
        _                 <-
          TestServer.addRoutes {
            Routes(
              Method.GET / "p2" / "move"                ->
                Handler.fromFunctionZIO { (request: Request) =>
                  ZIO.succeed(Response.text("Error:I'm server p2. I always crash."))
                },
              Method.GET / string("player-id") / "move" ->
                Handler
                  .fromFunctionZIO[(String, Request)] { case (playerID, request) =>
                    SamplePlayerRandomLogic.handleMove(request).delay(Duration.fromMillis(2))
                  }
                  .sandbox
            )
          }
        tournamentResults <-
          tournament
            .play(requestTimeout = Duration.fromMillis(200L))
            .provideSome[Client with Driver](TestServer.layer.and(Scope.default))
      yield tournamentResults).provide(
        TestServer.layer,
        ZLayer.succeed(Server.Config.default.onAnyOpenPort),
        Client.default,
        NettyDriver.customized,
        ZLayer.succeed(NettyConfig.defaultWithFastShutdown)
      )

      assertZIO(x)(
        Assertion.assertion("test") { results =>
          val j = TournamentResults.tournamentResultsEncoder.encodeJson(results, Some(1))
          // tournamentResultsEncoder
          // println(s"got => ${results}")
          // println(j)

          results.size3.nonEmpty &&
          results.size5.nonEmpty &&
          results.size7.nonEmpty
        }
      )
    } @@ TestAspect.withLiveClock
  )
