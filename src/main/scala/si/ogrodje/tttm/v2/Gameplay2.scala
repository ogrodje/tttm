package si.ogrodje.tttm.v2

import zio.*
import zio.http.*

import java.util.UUID
import zio.ZIO.logInfo
import zio.Console.printLine
import zio.prelude.NonEmptyList
import si.ogrodje.tttm.v2.Status.*

final class Gameplay2 private (
                                gid: GID,
                                serverA: PlayerServer,
                                serverB: PlayerServer
) {
  private type Servers = NonEmptyList[PlayerServer]
  private def swapServers(servers: Servers): Servers = NonEmptyList(servers.last, servers.head)

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
      case (err, err2)       => Left(new RuntimeException(s"Error parsing body - ${err} / ${err2}"))

  private def requestMove(client: Client, server: PlayerServer, game: Game): ZIO[Scope, Throwable, Move] = for
    request  <- mkMoveRequest(server.serverEndpoint, game)
    response <- client.request(request)
    move     <- response.body.asString.flatMap(body => ZIO.fromEither(parseBody(body)))
  yield move

  private def handleGame(client: Client, servers: Servers)(game: Game) =
    game.status match
      case Pending     => processRequest(client, servers = swapServers(servers), game = game.withSwitchPlaying)
      case Tide        => logInfo("Tide").as(game)
      case Won(symbol) => logInfo(s"Won by $symbol").as(game)

  private def processRequest(
    client: Client,
    servers: Servers,
    game: Game
  ): ZIO[Scope, Throwable, Game] =
    requestMove(client, servers.head, game).flatMap { move =>
      game.appendZIO(move).flatMap(handleGame(client, servers))
    }

  def play: ZIO[zio.Scope & Client, Throwable, Unit] = for {
    client <- ZIO.service[Client]
    result <- processRequest(client, NonEmptyList(serverA, serverB), Game.make(gid))
    _      <- printLine(s"Status: ${result.status}, moves: ${result.listMoves.length}\n${result.show}")
  } yield ()

}

object Gameplay2:
  def make(
            serverA: PlayerServer,
            serverB: PlayerServer,
            gid: GID = UUID.randomUUID()
  ): Gameplay2 = new Gameplay2(gid, serverA, serverB)
