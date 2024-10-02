package si.ogrodje.tttm.v2

import zio.Console.printLine
import zio.{Scope, Task, ZIO}
import zio.ZIO.logInfo
import zio.http.{Client, Path, Request, URL}
import Status.*
import java.time.LocalDateTime
import java.util.UUID

sealed trait GameplayEvent:
  def gid: GID
  def createdAt: LocalDateTime

final case class GameStarted(
  gid: GID,
  serverAEndpoint: ServerEndpoint,
  serverBEndpoint: ServerEndpoint,
  createdAt: LocalDateTime = LocalDateTime.now
) extends GameplayEvent

final case class RequestSent(
  gid: GID,
  serverEndpoint: ServerEndpoint,
  createdAt: LocalDateTime = LocalDateTime.now
) extends GameplayEvent

final case class ResponseReceived(
  gid: GID,
  serverEndpoint: ServerEndpoint,
  move: Move,
  createdAt: LocalDateTime = LocalDateTime.now
) extends GameplayEvent

final case class GameEnded(
  gid: GID,
  status: Status,
  createdAt: LocalDateTime = LocalDateTime.now
) extends GameplayEvent

final case class Gameplay private (
                                    gid: UUID,
                                    serverA: PlayerServer,
                                    serverB: PlayerServer
):
  private type Events = List[GameplayEvent]

  private def mkMoveRequest(endpoint: ServerEndpoint, game: Game): Task[Request] = for
    url         <- ZIO.fromEither(URL.decode(endpoint))
    queryParams <- ZIO.attempt(GameEncoder.encode(game))
    request = Request.get(url).updatePath(_ => Path("/move")).setQueryParams(queryParams)
  yield request

  private def parseBody(body: String): Either[Throwable, Move] =
    val maybeMove = "Move:([XO])-(\\d)-(\\d)".r
      .findFirstMatchIn(body)
      .map(m => m.group(1).charAt(0) -> (m.group(2).toInt, m.group(3).toInt))
      .toRight(new RuntimeException("No move found."))

    val maybeError = "Error:(.*)".r
      .findFirstMatchIn(body.take(255).trim)
      .map(_.group(1))
      .toRight(new RuntimeException("No error message found."))

    (maybeMove, maybeError) match
      case (Right(move), _)  => Right(move)
      case (_, Right(error)) => Left(new RuntimeException(error))
      case _                 => Left(new RuntimeException(s"Error parsing body."))

  private def callMove(
                        client: Client,
                        servers: List[PlayerServer],
                        game: Game,
                        events: Events
  ): ZIO[Scope, Throwable, (Game, Events)] = for
    server <- ZIO.fromOption(servers.headOption).orElseFail(new IllegalArgumentException("Missing first server"))
    (gid, serverEndpoint) = game.gid -> server.serverEndpoint
    _ <- logInfo(s"Calling move to ${server.show}")
    requestSent = RequestSent(gid, serverEndpoint)
    response           <- mkMoveRequest(serverEndpoint, game).flatMap(client.request)
    move @ (symbol, _) <- response.body.asString.flatMap(body => ZIO.fromEither(parseBody(body)))

    _ <- ZIO.when(symbol != game.playing)(ZIO.fail(new RuntimeException("Server replied with invalid symbol")))
    _ <- logInfo(s"Replied with ${move}")
    responseReceived = ResponseReceived(gid, serverEndpoint, move)

    moveEvents = events ++ List(requestSent, responseReceived)
    newGame <- ZIO.fromEither(game.append(move))

    response <- newGame.status match
      case status @ Won(symbol) =>
        logInfo(s"Symbol: $symbol has won.").as(
          newGame -> (moveEvents :+ GameEnded(game.gid, status))
        )
      case status @ Tide        =>
        logInfo("Game ended with tide").as(
          newGame -> (moveEvents :+ GameEnded(game.gid, status))
        )
      case Pending              =>
        logInfo("Doing another move") *>
          callMove(
            client,
            servers = servers.reverse,
            game = newGame.withSwitchPlaying,
            moveEvents
          )
  yield response

  def play(): ZIO[Scope & Client, Throwable, Unit] = for
    client <- ZIO.service[Client]
    game = Game.make(gid)
    _                   <- logInfo(s"Playing ${serverA.show} VS. ${serverB.show}, gid: ${game.gid}")
    (finalGame, events) <- callMove(
      client,
      serverA :: serverB :: Nil,
      game,
      events = List(
        GameStarted(gid, serverAEndpoint = serverA.serverEndpoint, serverBEndpoint = serverB.serverEndpoint)
      )
    )
    _                   <- logInfo(s"Status: ${finalGame.status}")
    _                   <- printLine(s"Final game:\n${finalGame.show}")
  // _                   <- printLine(events.mkString("\n"))
  yield ()

object Gameplay:
  def make(serverA: PlayerServer, serverB: PlayerServer): Gameplay = Gameplay(gid = UUID.randomUUID(), serverA, serverB)
