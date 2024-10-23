package si.ogrodje.tttm.v2.apps

import si.ogrodje.tttm.v2.apps.MainApp.validSizes
import si.ogrodje.tttm.v2.persistance.{DB, DBConfiguration, TournamentResultsDAO}
import si.ogrodje.tttm.v2.*
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

    _ <- logInfo(s"Tournament with ID: $id has completed.")

    scores <-
      ZStream
        .fromIterable(sizes)
        .mapZIOParUnordered(3)(size =>
          ZIO.succeed(Scoring.forSize(tournamentResults, size)(using explain = false)).map(r => size -> r)
        )
        .tap((size, scores) =>
          zio.Console.printLine(
            s"\n⚡️ SIZE: ${size}, SCORES:\n${scores.toList.sortBy(-_._2).map((id, s) => id + " = " + s).mkString("\n")}"
          )
        )
        .runDrain

    _ <-
      ZIO.foreachDiscard(maybeWriteTo) { path =>
        ZIO.attemptBlocking(
          Files.write(path, json.toString.getBytes(StandardCharsets.UTF_8))
        )
      }
  yield ()).provide(
    RichClient.live.and(Scope.default),
    ZLayer.fromZIO(DBConfiguration.fromEnvironment.flatMap(DB.transactor.make)),
    DBConfiguration.live
  )
