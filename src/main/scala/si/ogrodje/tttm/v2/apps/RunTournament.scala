package si.ogrodje.tttm.v2.apps

import si.ogrodje.tttm.v2.apps.MainApp.validSizes
import si.ogrodje.tttm.v2.persistance.{DB, DBConfiguration, TournamentResultsDAO}
import si.ogrodje.tttm.v2.*
import si.ogrodje.tttm.v2.scoring.Scoring
import zio.ZIO.logInfo
import zio.json.*
import zio.stream.ZStream
import zio.{Duration, Scope, Task, ZIO, ZLayer}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

object RunTournament:

  def run(
    numberOfGames: Int,
    maybeRawSize: Option[Int],
    storeResults: Boolean,
    explainScoring: Boolean,
    maybeWriteTo: Option[Path]
  ): Task[Unit] = (for
    _ <-
      logInfo(
        s"Starting tournament with number of games: $numberOfGames, " +
          s"storing results: $storeResults, " +
          s"write: ${maybeWriteTo.map(_.toAbsolutePath)}"
      )

    playersConfig <- PlayersConfig.fromDefaultFile
    sizes         <- validSizes(maybeRawSize).orElseSucceed(Sizes.validSizes)

    // Run migration if persistence is needed
    _ <- ZIO.when(storeResults)(DB.loadMigrate)

    // Run tournament
    tournamentResults <-
      Tournament
        .fromPlayersConfig(playersConfig, sizes, numberOfGames = numberOfGames)
        .play(requestTimeout = Duration.fromSeconds(2L))

    // Scoring
    tournamentScores  <- Scoring.forTournament(tournamentResults)
    finalResults       = tournamentResults.copyWithScores(tournamentScores)

    // Persist
    _   <-
      ZIO.when(storeResults)(TournamentResultsDAO.save(finalResults))

    // Reporting
    _   <- logInfo(s"Tournament with ID: ${tournamentResults.id} has completed. Computing scores")
    json = finalResults.toJsonPretty
    _   <- zio.Console.printLine(json)

    // Dumping to JSON
    _ <-
      ZIO.foreachDiscard(maybeWriteTo) { path =>
        ZIO.attemptBlocking(
          Files.write(path, json.getBytes(StandardCharsets.UTF_8))
        )
      }
  yield ()).provide(
    RichClient.live.and(Scope.default),
    ZLayer.fromZIO(DBConfiguration.fromEnvironment.flatMap(DB.transactor.make)),
    DBConfiguration.live
  )
