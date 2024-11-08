package si.ogrodje.tttm.v2.persistance

import doobie.*
import doobie.implicits.*
import si.ogrodje.tttm.v2.persistance.DB.TransactorTask
import si.ogrodje.tttm.v2.scoring.{Score, Scores}
import si.ogrodje.tttm.v2.*
import zio.*
import zio.interop.catz.*
import doobie.Fragment
import doobie.postgres.*
import doobie.postgres.implicits.*
import si.ogrodje.tttm.v2.Status.*

object GamesDAO:
  given Put[Status]       = Put[String].contramap {
    case Tie             => "Tie"
    case Pending         => "Pending"
    case Won(_)          => "Won"
    case CrashedBy(_, _) => "Crashed"
  }
  given Put[Size]         = Put[String].contramap(_.value.toString)
  given Put[ServerResult] = Put[String].contramap {
    case sr @ ServerResult(responseAverage, responseMedian, responseP99, responseMin, responseMax, numberOfMoves) =>
      s"($responseAverage, " +
        s"$responseMedian," +
        s"$responseP99," +
        s"$responseMin," +
        s"$responseMax," +
        s"$numberOfMoves" +
        s")"
  }

  private object queries:

    private def crashedByFromStatus(game: Game, status: Status): Option[String] = status match
      case CrashedBy(X, _) => Some(game.playerServerIDX)
      case CrashedBy(O, _) => Some(game.playerServerIDO)
      case _               => None

    private def crashedMessage(status: Status): Option[String] = status match
      case CrashedBy(_, message) => Some(message)
      case _                     => None

    private def movesToArray(moves: List[Move]): Fragment =
      val inner = moves.collect { case Move(symbol, (row, column), duration, Some(playerServerID)) =>
        s"('$symbol', ($row, $column), '$playerServerID', ${duration.toMillis.toDouble})"
      }.mkString(", ")
      Fragment.const(s"ARRAY[$inner] :: GAME_MOVE[]")

    val insertGame: (TournamentID, Game, GameplayResult) => Update0 =
      case (
            tournamentID,
            game @ Game(gid, playerServerIDX, playerServerIDO, _, _, size, crashedBy),
            GameplayResult(_, duration, status, maybeWinner, serverAResult, serverBResult, moves)
          ) =>
        (sql"""INSERT INTO games (
                id, tournament_id, server_a, server_b, server_a_result, server_b_result,
                status, size, maybe_winner, crashed_by, crashed_message, duration_ms,
                moves
              ) VALUES (
                $gid, 
                $tournamentID, 
                $playerServerIDX, 
                $playerServerIDO,
                $serverAResult :: SERVER_RESULT,
                $serverBResult :: SERVER_RESULT,
                $status :: STATUS,
                $size :: SIZE,
                $maybeWinner,
                ${crashedByFromStatus(game, status)},
                ${crashedMessage(status)},
                ${duration.toMillis.toDouble},
                """ ++ movesToArray(moves) ++ sql"""
              )
             """).updateWithLabel("inserting-game")

  def save(tournamentID: TournamentID, game: Game, gameplayResult: GameplayResult): RIO[TransactorTask, GID] =
    ZIO.serviceWithZIO[TransactorTask](queries.insertGame(tournamentID, game, gameplayResult).run.transact).as(game.gid)
