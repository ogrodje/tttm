package si.ogrodje.tttm.v2

import si.ogrodje.tttm.v2
import si.ogrodje.tttm.v2.Status.*
import zio.*
import zio.http.*
import zio.prelude.NonEmptyList

import java.util.UUID

final class Gameplay private (
  gid: GID,
  serverA: PlayerServer,
  serverB: PlayerServer,
  size: Size = Size.default,
  reporter: GameplayReporter
):
  private type Servers = NonEmptyList[PlayerServer]
  private def swapServers(servers: Servers): Servers = NonEmptyList(servers.last, servers.head)

  private def mkMoveRequest(endpoint: ServerEndpoint, game: Game): Task[Request] = for
    url         <- ZIO.fromEither(URL.decode(endpoint))
    queryParams <- ZIO.attempt(GameEncoder.encode(game))
    request      = Request.get(url).updatePath(_ => Path("/move")).setQueryParams(queryParams)
    _           <- reporter.logInfo(s"Requesting ${request.url.host}")
  yield request

  private def requestMove(client: Client, server: PlayerServer, game: Game): ZIO[Scope, Throwable, Move] = for
    request              <- mkMoveRequest(server.serverEndpoint, game)
    (duration, response) <- client.request(request).timed
    move                 <- response.body.asString.flatMap(BodyParser.parse)
    _                    <- reporter.logInfo(s"Received response from ${request.url.host} in ${duration.toMillis}ms")
  yield move

  private def handleGame(client: Client, servers: Servers)(
    game: Game
  ): ZIO[Scope, Throwable, Game] =
    game.status match
      case Pending     =>
        processRequest(
          client,
          servers = swapServers(servers),
          game = game.withSwitchPlaying
        )
      case Tide        => reporter.logInfo("Tide").as(game)
      case Won(symbol) => reporter.logInfo(s"Won by $symbol").as(game)

  private def processRequest(
    client: Client,
    servers: Servers,
    game: Game
  ): ZIO[Scope, Throwable, Game] =
    requestMove(client, servers.head, game).flatMap { move =>
      game.appendZIO(move).flatMap(handleGame(client, servers))
    }

  def play: ZIO[zio.Scope & Client, Throwable, (Game, Option[PlayerServer])] = for
    client     <- ZIO.service[Client]
    servers     = NonEmptyList(serverA, serverB)
    result     <- processRequest(client, servers, Game.make(gid, size = size))
    _          <- reporter.logInfo(s"Game completed. Status: ${result.status}, moves: ${result.moves.length}")
    maybeWinner = result.status match
                    case Won(`X`) => Some(serverA)
                    case Won(`O`) => Some(serverB)
                    case _        => None
  yield result -> maybeWinner

object Gameplay:
  def make(
    serverA: PlayerServer,
    serverB: PlayerServer,
    size: Size = Size.default,
    gid: GID = UUID.randomUUID(),
    maybeGameplayReporter: Option[GameplayReporter] = None
  ): Gameplay = new Gameplay(
    gid,
    serverA,
    serverB,
    size,
    maybeGameplayReporter.getOrElse(
      CLIGameReporter.make(gid)
    )
  )
