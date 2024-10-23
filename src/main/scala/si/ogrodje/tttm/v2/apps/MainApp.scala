package si.ogrodje.tttm.v2.apps

import si.ogrodje.tttm.v2.*
import zio.*
import zio.Console.printLine
import zio.ZIO.logInfo
import zio.cli.*
import zio.http.{Client, URL}
import zio.logging.backend.SLF4J
import zio.prelude.*
import zio.json.*
import eu.timepit.refined.auto.*
import java.net.MalformedURLException
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

object MainApp extends ZIOCliDefault:
  private def parseURL(raw: String): IO[MalformedURLException, URL] = ZIO.fromEither(URL.decode(raw))

  override val bootstrap: ZLayer[ZIOAppArgs, Nothing, Any] = Runtime.removeDefaultLoggers >>> SLF4J.slf4j

  enum Subcommand:
    case ServerCommand(port: BigInt) extends Subcommand
    case TournamentCommand(
      numberOfGames: BigInt,
      size: Option[Int],
      storeResult: Boolean,
      writeTo: Option[Path]
    )                                extends Subcommand
    case PlayCommand(
      size: BigInt,
      numberOfGames: BigInt,
      serverA: String,
      serverB: String
    )                                extends Subcommand

  import Subcommand.*

  // Server
  private type ServerCommandArgs = BigInt
  private val serverCommand: Command[ServerCommandArgs] = Command(
    "server",
    Options.integer("port").alias("P").withDefault(BigInt(7777)),
    Args.none
  ).withHelp(HelpDoc.p("Runs the main tttm server"))
  private def validateServerCommand(
    serverCommand: ServerCommand
  ): ZValidation[Nothing, String, Int] =
    Validation.succeed(serverCommand.port.toInt)

  // Tournament
  private type TournamentCommandArgs = (BigInt, Option[Int], Boolean, Option[Path])
  private val tournamentCommand: Command[TournamentCommandArgs] = Command(
    "tournament",
    Options.integer("number-of-games").alias("ng").withDefault(BigInt(10)) ++
      Options
        .enumeration[Option[Int]]("size")("3" -> Some(3), "5" -> Some(5), "7" -> Some(7), "All" -> None)
        .alias("s")
        .withDefault(None) ++
      Options.boolean("store-results") ++
      Options.file("write-to").alias("out").optional,
    Args.none
  ).withHelp(HelpDoc.p("Play the tournament between players"))

  private def validateTournamentCommand(
    tournamentCommand: TournamentCommand
  ): ZValidation[Nothing, Nothing, (Int, Option[Int], Boolean, Option[Path])] =
    Validation.validate(
      Validation.succeed(tournamentCommand.numberOfGames.toInt),
      Validation.succeed(tournamentCommand.size),
      Validation.succeed(tournamentCommand.storeResult),
      Validation.succeed(tournamentCommand.writeTo)
    )

  // Play
  private type PlayCommandArgs = ((BigInt, BigInt), (String, String))
  private val playCommand: Command[PlayCommandArgs] = Command(
    "play",
    Options.integer("size").alias("s").withDefault(BigInt(3)) ++
      Options.integer("games").alias("g").withDefault(BigInt(1)),
    Args.text("server-a") ++ Args.text("server-b")
  ).withHelp(HelpDoc.p("Play games between two servers"))

  private def validatePlayCommand(
    playCommand: PlayCommand
  ): ZValidation[Nothing, String, (URL, URL, Size, Int)] =
    Validation.validate(
      Validation.fromEither(URL.decode(playCommand.serverA).left.map(_.toString)),
      Validation.fromEither(URL.decode(playCommand.serverB).left.map(_.toString)),
      Validation.fromEither(Size.safe(playCommand.size.toInt).left.map(_.message)),
      Validation.succeed(playCommand.numberOfGames.toInt)
    )

  private val runCommand: Command[Unit] = Command("run").withHelp(HelpDoc.p("Run the sub-command"))

  private val command = runCommand.subcommands(
    serverCommand.map(ServerCommand.apply),
    tournamentCommand.map(TournamentCommand.apply),
    playCommand.map { case ((rSize, rGames), (rServerA, rServerB)) => PlayCommand(rSize, rGames, rServerA, rServerB) }
  )

  val cliApp = CliApp.make(
    name = "TTTM Main App",
    version = "0.0.1",
    summary = HelpDoc.Span.text("Main entrypoint for tttm services."),
    command = command
  ) {
    case cmd: TournamentCommand => validateTournamentCommand(cmd).toZIO.flatMap(RunTournament.run).as(Exit.Success)
    case cmd: PlayCommand       => validatePlayCommand(cmd).toZIO.flatMap(play).as(Exit.Success)
    case cmd: ServerCommand     => validateServerCommand(cmd).toZIO.flatMap(server).as(Exit.Success)
  }

  // Actual commands beyond this point.
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

  def validSizes(maybeRawSize: Option[Int]): IO[RuntimeException, Sizes] =
    ZIO.from(
      maybeRawSize
        .toRight(new RuntimeException("Missing size"))
        .flatMap(Size.safe)
        .flatMap(s => Sizes.safe(s :: Nil).left.map(er => new RuntimeException(s"Problem with size: ${er}")))
    )

  private def server(port: Int): Task[Unit] = ServerApp.runWithPort(port)
