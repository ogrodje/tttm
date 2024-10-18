package si.ogrodje.tttm.v2

import si.ogrodje.tttm.v2.GameplayError.*
import si.ogrodje.tttm.v2.Status.*
import zio.*
import zio.http.*
import zio.prelude.NonEmptyList

import java.util.UUID

enum GameplayError(playerServer: PlayerServer, val message: String) extends Throwable(message):
  case RequestBuildingError(playerServer: PlayerServer, override val message: String)
      extends GameplayError(playerServer, s"[${playerServer.id}] Request building has failed with $message")
  case ServerTimeout(playerServer: PlayerServer, serverURL: String)
      extends GameplayError(playerServer, s"[${playerServer.id}] Server $serverURL has timed-out.")
  case ParsingError(playerServer: PlayerServer, override val message: String)
      extends GameplayError(playerServer, s"[${playerServer.id}] Gameplay error: $message")
  case NetworkingError(playerServer: PlayerServer, override val message: String)
      extends GameplayError(playerServer, s"[${playerServer.id}] Networking or protocol error: $message")

final class Gameplay private (
  gid: GID,
  serverA: PlayerServer,
  serverB: PlayerServer,
  size: Size = Size.default,
  reporter: GameplayReporter,
  requestTimeout: Duration = Duration.fromSeconds(2)
):
  private type Servers = NonEmptyList[PlayerServer]
  private def swapServers(servers: Servers): Servers = NonEmptyList(servers.last, servers.head)

  private def mkMoveRequest(playerServer: PlayerServer, game: Game): Task[Request] = for
    queryParams <- GameEncoder.encodeZIO(game)
    endpoint     = playerServer.serverEndpoint
    request      = Request.get(url = endpoint).updatePath(_ ++ Path("/move")).setQueryParams(queryParams)
    _           <-
      reporter.logInfo(s"Requesting ${request.url.host.getOrElse("UNKNOWN")}")(
        Some(game.gid)
      )
  yield request

  private def requestMove(client: Client, server: PlayerServer, game: Game): ZIO[Scope, GameplayError, Move] = for
    request                   <-
      mkMoveRequest(server, game).mapError(th => RequestBuildingError(server, th.getMessage))
    (duration, maybeResponse) <-
      client
        .request(request)
        .timeout(requestTimeout)
        .timed
        .mapError(th => NetworkingError(server, s"Networking error with ${th.getMessage}"))
    response                  <-
      ZIO.fromOption(maybeResponse).orElseFail(ServerTimeout(server, server.serverEndpoint.toString))
    move                      <-
      response.body.asString
        .flatMap(BodyParser.parse)
        .mapError(th => ParsingError(server, s"Player server has failed with: ${th.getMessage}"))
    _                         <-
      reporter.logInfo(
        s"Received valid response from ${server.id} in ${duration.toMillis}ms"
      )(
        Some(game.gid)
      )
  yield move.copy(duration = duration, playerServerID = Some(server.id))

  private def handleGame(client: Client, servers: Servers)(
    game: Game
  ): ZIO[Scope, Throwable, Game] =
    game.status match
      case Pending                    =>
        processRequest(
          client,
          servers = swapServers(servers),
          game = game.copyBySwitchingPlaying
        )
      case Tie                        => reporter.logInfo("Tie")(Some(game.gid)).as(game)
      case Won(symbol)                => reporter.logInfo(s"Won by $symbol")(Some(game.gid)).as(game)
      case CrashedBy(symbol, message) =>
        reporter.logInfo(s"Crashed by $symbol with $message")(Some(game.gid)).as(game)

  private def processRequest(
    client: Client,
    servers: Servers,
    game: Game
  ): ZIO[Scope, Throwable, Game] = for
    moveOrCrash <- requestMove(client, servers.head, game).either
    out         <-
      moveOrCrash match
        case Right(move)         =>
          for
            mutatedGame <- game.appendZIO(move)
            game        <- handleGame(client, servers)(mutatedGame)
          yield game
        case Left(gameplayError) =>
          ZIO.succeed(game.copyAsCrashed(gameplayError.getMessage))
  yield out

  def play: ZIO[zio.Scope & Client, Throwable, (Game, GameplayResult)] = for
    client           <- ZIO.service[Client]
    servers           = NonEmptyList(serverA, serverB)
    game              = Game.make(gid, playerServerIDX = serverA.id, playerServerIDO = serverB.id, size = size)
    (duration, game) <- processRequest(client, servers, game).timed
    _                <-
      reporter.logInfo(s"Game completed. Status: ${game.status}. Number of moves: ${game.moves.length}")(
        Some(game.gid)
      )
  yield game -> GameplayResult.fromGame(servers, duration, game)

object Gameplay:
  def make(
    serverA: PlayerServer,
    serverB: PlayerServer,
    size: Size = Size.default,
    gid: GID = UUID.randomUUID(),
    maybeGameplayReporter: Option[GameplayReporter] = None,
    requestTimeout: Duration = Duration.fromSeconds(2)
  ): Gameplay = new Gameplay(
    gid,
    serverA,
    serverB,
    size,
    maybeGameplayReporter.getOrElse(CLIGameReporter.make(gid)),
    requestTimeout
  )
