package si.ogrodje.tttm.v2.apps

import si.ogrodje.tttm.v2.{ExternalPlayerServer, Gameplay, Match, MatchResult, PlayerServer, Size}
import zio.ZIO.logInfo
import zio.cli.*
import zio.http.{Client, URL}
import zio.logging.backend.SLF4J
import zio.*
import zio.json.*
import zio.stream.{Stream, ZStream}
import zio.Console.printLine

import java.net.MalformedURLException

type RawURL = String
object RawURL:
  def parse(raw: RawURL): IO[MalformedURLException, URL] = ZIO.fromEither(URL.decode(raw))

object PlayApp extends ZIOCliDefault:
  override val bootstrap: ZLayer[ZIOAppArgs, Nothing, Any] =
    Runtime.removeDefaultLoggers >>> SLF4J.slf4j

  // private type Size          = BigInt
  private type ShouldScore   = Boolean
  private type NumberOfGames = BigInt
  private val score: Options[ShouldScore]           = Options.boolean("score").alias("s")
  private val size: Options[BigInt]                 = Options.integer("size").withDefault(BigInt(3))
  private val numberOfGames: Options[NumberOfGames] = Options.integer("games").alias("g").withDefault(BigInt(1))
  private val help: HelpDoc                         = HelpDoc.p("Plays a game between two servers.")

  private val (serverA: Args[RawURL], serverB: Args[RawURL]) =
    Args.text("server-a") -> Args.text("server-b")

  private val command: Command[((ShouldScore, BigInt, NumberOfGames), (RawURL, RawURL))] =
    Command("play", score ++ size ++ numberOfGames, serverA ++ serverB).withHelp(help)

  val cliApp = CliApp.make(
    name = "PlayApp",
    version = "0.0.1",
    summary = HelpDoc.Span.empty,
    command = command
  ) { case ((shouldScore, rawSize, numberOfGames), _ @(serverA, serverB)) =>
    for
      (serverA, serverB) <- RawURL.parse(serverA).zipPar(RawURL.parse(serverB))
      size               <- ZIO.fromEither(Size.of(rawSize.toInt))
      out                <- play(size, shouldScore, numberOfGames)(serverA, serverB)
    yield out
  }

  private def play(
    size: Size,
    shouldScore: ShouldScore = false,
    numberOfGames: BigInt = BigInt(1)
  )(serverAUrl: URL, serverBUrl: URL) = for
    _      <- logInfo(s"Server A: $serverAUrl, server B: $serverBUrl, size: $size, should score: $shouldScore")
    serverA = ExternalPlayerServer.unsafeFromURL(serverAUrl)
    serverB = ExternalPlayerServer.unsafeFromURL(serverBUrl)
    out    <- Match
                .mk(serverA, serverB, numberOfGames.toLong, size)
                .playGames(concurrentProcesses = 3)
                .provide(Client.default.and(Scope.default))
    _      <- printLine(MatchResult.matchResultJsonEncoder.encodeJson(out, Some(2)))
  yield ()
