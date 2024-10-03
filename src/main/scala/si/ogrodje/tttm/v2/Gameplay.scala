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

  private def mkMoveRequest(playerServer: PlayerServer, game: Game): Task[Request] = for
    queryParams <- ZIO.attempt(GameEncoder.encode(game))
    endpoint     = playerServer.serverEndpoint
    request      = Request.get(url = endpoint).updatePath(_ ++ Path("/move")).setQueryParams(queryParams)
    _           <- reporter.logInfo(s"Requesting ${request.url.host}")
  yield request

  private def requestMove(client: Client, server: PlayerServer, game: Game): ZIO[Scope, Throwable, Move] = for
    request              <- mkMoveRequest(server, game)
    (duration, response) <- client.request(request).timed
    move                 <- response.body.asString.flatMap(BodyParser.parse)
    _                    <- reporter.logInfo(s"Received response from ${request.url.host} in ${duration.toMillis}ms")
  yield move.copy(duration = duration)

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
      case Tie        => reporter.logInfo("Tie").as(game)
      case Won(symbol) => reporter.logInfo(s"Won by $symbol").as(game)

  private def processRequest(
    client: Client,
    servers: Servers,
    game: Game
  ): ZIO[Scope, Throwable, Game] =
    requestMove(client, servers.head, game).flatMap { move =>
      game.appendZIO(move).flatMap(handleGame(client, servers))
    }

  def play: ZIO[zio.Scope & Client, Throwable, (Game, GameplayResult)] = for
    client           <- ZIO.service[Client]
    servers           = NonEmptyList(serverA, serverB)
    (duration, game) <- processRequest(client, servers, Game.make(gid, size = size)).timed
    _                <- reporter.logInfo(s"Game completed. Status: ${game.status}, moves: ${game.moves.length}")
  yield game -> GameplayResult.fromGame(servers, duration, game)

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
    maybeGameplayReporter.getOrElse(CLIGameReporter.make(gid))
  )
