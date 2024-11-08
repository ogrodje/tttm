package si.ogrodje.tttm.v2.persistance

import si.ogrodje.tttm.v2.persistance.DB.TransactorTask
import si.ogrodje.tttm.v2.{Game, GameplayResult, TournamentID}
import zio.ZIO.{logError, logInfo}
import zio.stream.ZStream
import zio.{Queue, UIO, ZIO}

type GameplayResultQueue = Queue[(Game, GameplayResult) | GameplayResultQueue.Done]

object GameplayResultQueue:
  val Done = ""
  type Done    = String
  type Payload = (Game, GameplayResult)
  def mk: UIO[GameplayResultQueue] = Queue.unbounded[Payload | GameplayResultQueue.Done]

final class GameplayResultSink private (
  private val tournamentID: TournamentID,
  private val resultsQueue: GameplayResultQueue
):

  def observe: ZIO[TransactorTask, Throwable, Unit] =
    ZStream
      .fromQueueWithShutdown(resultsQueue)
      .takeUntil {
        case GameplayResultQueue.Done => true
        case _                        => false
      }
      .tap {
        case (game: Game, gameplayResult: GameplayResult) =>
          GamesDAO
            .save(tournamentID, game, gameplayResult)
            .tapBoth(
              err => logError(s"Failed to persist game: $err"),
              gid => logInfo(s"Persisted game $gid")
            )
        case _                                            => ZIO.unit
      }
      .runDrain

object GameplayResultSink:
  def make(tournamentID: TournamentID, queue: GameplayResultQueue): ZIO[Any, Nothing, GameplayResultSink] =
    ZIO.succeed(new GameplayResultSink(tournamentID, queue))
