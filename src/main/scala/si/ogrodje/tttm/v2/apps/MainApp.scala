package si.ogrodje.tttm.v2.apps

import si.ogrodje.tttm.v2.{ExternalPlayerServer, Match, MatchResult, PlayersConfig, Size, Tournament}
import zio.*
import zio.cli.*
import zio.ZIO.logInfo
import zio.cli.*
import zio.http.{Client, URL}
import zio.logging.backend.SLF4J
import zio.*
import zio.json.*
import zio.stream.{Stream, ZStream}
import zio.Console.printLine
import zio.prelude.*
import eu.timepit.refined.auto.*

import java.net.MalformedURLException

object MainApp extends ZIOCliDefault:
  private def parseURL(raw: String): IO[MalformedURLException, URL] = ZIO.fromEither(URL.decode(raw))

  override val bootstrap: ZLayer[ZIOAppArgs, Nothing, Any] = Runtime.removeDefaultLoggers >>> SLF4J.slf4j

  private val serverCommand: Command[Unit] = Command(
    "server",
    Options.none,
    Args.none
  ).withHelp(HelpDoc.p("Runs the main tttm server"))

  private type TournamentCommandArgs = (BigInt, Boolean)
  private val tournamentCommand: Command[TournamentCommandArgs] = Command(
    "tournament",
    Options.integer("number-of-games").alias("ng").withDefault(BigInt(10)) ++
      Options.boolean("store-results"),
    Args.none
  ).withHelp(HelpDoc.p("Play the tournament between players"))
  private def validateTournamentArgs(
    tournamentCommandArgs: TournamentCommandArgs
  ) =
    val (rawNumberOfGames, rawStore) = tournamentCommandArgs
    Validation.validateWith(
      Validation.succeed(rawNumberOfGames.toInt),
      Validation.succeed(rawStore)
    ) { case ops @ (_, _) => ops }

  private type PlayCommandArgs = ((BigInt, BigInt), (String, String))
  private val playCommand: Command[PlayCommandArgs] = Command(
    "play",
    Options.integer("size").alias("s").withDefault(BigInt(3)) ++
      Options.integer("games").alias("g").withDefault(BigInt(1)),
    Args.text("server-a") ++ Args.text("server-b")
  ).withHelp(HelpDoc.p("Play games between two servers"))

  private def validatePlayArgs(playCommandArgs: PlayCommandArgs): Validation[String, (URL, URL, Size, Int)] =
    val ((rawSize, rawGames), (rawServerA, rawServerB)) = playCommandArgs
    Validation.validateWith(
      Validation.fromEither(URL.decode(rawServerA).left.map(_.toString)),
      Validation.fromEither(URL.decode(rawServerB).left.map(_.toString)),
      Validation.fromEither(Size.safe(rawSize.toInt).left.map(_.message)),
      Validation.succeed(rawGames.toInt)
    ) { case pom @ (serverAUrl, serverBUrl, size, numberOfGames) => pom }

  private val runCommand: Command[Unit] = Command("run").withHelp(HelpDoc.p("Run the sub-command"))

  enum Subcommand:
    case ServerCommand                                                                      extends Subcommand
    case TournamentCommand(numberOfGames: BigInt, storeResult: Boolean)                     extends Subcommand
    case PlayCommand(size: BigInt, numberOfGames: BigInt, serverA: String, serverB: String) extends Subcommand

  import Subcommand.*

  private val command = runCommand.subcommands(
    serverCommand.map(_ => ServerCommand),
    tournamentCommand.map((s, st) => TournamentCommand(s, st)),
    playCommand.map { case ((rSize, rGames), (rServerA, rServerB)) => PlayCommand(rSize, rGames, rServerA, rServerB) }
  )

  val cliApp = CliApp.make(
    name = "TTTM Main App",
    version = "0.0.1",
    summary = HelpDoc.Span.text("Main entrypoint for tttm services."),
    command = command
  ) {
    case TournamentCommand(numberOfGames, storeResult)      =>
      validateTournamentArgs((numberOfGames, storeResult)).toZIO.flatMap(tournament).as(Exit.Success)
    case PlayCommand(size, numberOfGames, serverA, serverB) =>
      validatePlayArgs(((size, numberOfGames), (serverA, serverB))).toZIO.flatMap(play).as(Exit.Success)
    case other                                              =>
      ZIO.logInfo(s"Other command with ${other}").as(Exit.Failure)
  }

  private def play(serverAUrl: URL, serverBUrl: URL, size: Size, numberOfGames: Int): Task[Unit] = for
    _      <- logInfo(s"Server A: $serverAUrl, server B: $serverBUrl, size: $size")
    serverA = ExternalPlayerServer.unsafeFromURL(serverAUrl)
    serverB = ExternalPlayerServer.unsafeFromURL(serverBUrl)
    out    <-
      Match
        .mk(serverA, serverB, numberOfGames.toLong, size)
        .playGames(concurrentProcesses = 3)
        .provide(Client.default.and(Scope.default))
    _      <- printLine(MatchResult.matchResultJsonEncoder.encodeJson(out, Some(1)))
  yield ()

  private def tournament(numberOfGames: Int, storeResults: Boolean): Task[Unit] = for
    _             <- logInfo(s"Starting tournament with number of games: ${numberOfGames}, storing results: ${storeResults}")
    playersConfig <- PlayersConfig.fromDefaultFile
    tournament     = Tournament.fromPlayersConfig(playersConfig, numberOfGames = numberOfGames)
    result        <- tournament
                       .play(requestTimeout = Duration.fromSeconds(2L))
                       .provide(Client.default.and(Scope.default))
  yield ()
