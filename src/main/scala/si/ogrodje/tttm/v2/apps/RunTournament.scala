package si.ogrodje.tttm.v2.apps

import si.ogrodje.tttm.v2.{PlayersConfig, Sizes, Tournament, TournamentResults}
import si.ogrodje.tttm.v2.apps.MainApp.validSizes
import si.ogrodje.tttm.v2.persistance.{DB, DBConfiguration, TournamentResultsDAO}
import zio.{Duration, Scope, Task, ZIO, ZLayer}
import zio.ZIO.logInfo
import zio.http.Client
import zio.json.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

object RunTournament:

  def run(
    numberOfGames: Int,
    maybeRawSize: Option[Int],
    storeResults: Boolean,
    maybeWriteTo: Option[Path]
  ): Task[Unit] = (for
    _             <-
      logInfo(
        s"Starting tournament with number of games: $numberOfGames, " +
          s"storing results: $storeResults, " +
          s"write: ${maybeWriteTo.map(_.toAbsolutePath)}"
      )
    playersConfig <- PlayersConfig.fromDefaultFile
    sizes         <- validSizes(maybeRawSize).orElseSucceed(Sizes.validSizes)

    tournamentResults <-
      Tournament
        .fromPlayersConfig(playersConfig, sizes, numberOfGames = numberOfGames)
        .play(requestTimeout = Duration.fromSeconds(2L))

    _  <- DB.loadMigrate
    id <- TournamentResultsDAO.saveTournamentResults(tournamentResults)

    json = TournamentResults.tournamentResultsEncoder.encodeJson(tournamentResults, None)
    _   <- zio.Console.printLine(tournamentResults.toJsonPretty)

    _ <-
      ZIO.foreachDiscard(maybeWriteTo) { path =>
        ZIO.attemptBlocking(
          Files.write(path, json.toString.getBytes(StandardCharsets.UTF_8))
        )
      }
  yield ()).provide(
    Client.default.and(Scope.default),
    ZLayer.fromZIO(DBConfiguration.fromEnvironment.flatMap(DB.transactor.make)),
    DBConfiguration.live
  )
