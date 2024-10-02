package si.ogrodje.tttm.v1

import zio.*
import zio.Console.*
import zio.http.*
import zio.prelude.NonEmptyList
import ZIO.{logInfo, logWarning}
import zio.logging.backend.SLF4J

import java.util.UUID
import scala.util.Try

type ServerEndpoint = String

final class GameServer private (port: Int):
  private val routes =
    Routes(
      Method.GET / Root   -> handler(Response.text("Nothing here. I'm just a server playing the game.")),
      Method.GET / "move" -> handler { (req: Request) =>
        for {
          game <- Game.fromQueryParams(req.queryParameters)
          _    <- printLine(game)
          response = Response.text(s"Game: $game")
        } yield response
      }.sandbox
    )

  private val serverEndpoint: ServerEndpoint = s"http://0.0.0.0:$port/"
  def run: Task[Nothing]                     = Server.serve(routes).provide(Server.defaultWithPort(port))

  def runAnd[A](cb: ServerEndpoint => ZIO[Any, Throwable, A]): ZIO[Any, Throwable, Unit] = {
    val server = Server.serve(routes).provide(Server.defaultWithPort(port)).onExit {
      case Exit.Success(_)     => logInfo("Server started successfully!")
      case Exit.Failure(cause) => logWarning(s"Server failure ${cause.prettyPrint}")
    }

    ZIO.scoped {
      for
        f <- server.fork
        _ <- cb(serverEndpoint)
        _ <- f.join
      yield ()
    }
  }

object GameServer:
  def mkAndRun(port: Int): Task[GameServer]                                    = new GameServer(port).run
  def mkAndRunWith[A](port: Int)(cb: ServerEndpoint => ZIO[Any, Throwable, A]) = new GameServer(port).runAnd(cb)

type GameID   = UUID
type Symbol   = Char
type Position = (Int, Int)
type Move     = (Symbol, Position)
type Size     = Int
final case class Game private (gameID: GameID, size: Size = 3, moves: Seq[Move] = Seq.empty):
  def asQueryParams: Map[String, Chunk[String]] = Map(
    "gid"   -> Chunk.apply(gameID.toString),
    "size"  -> Chunk.apply(size.toString),
    "moves" -> Chunk.apply(moves.map { case (symbol, (x, y)) => s"$symbol,$x,$y" }.mkString(";"))
  )

  def withMove(move: Move): Game = copy(moves = moves ++ Seq(move))

object Game:
  def mkEmpty(gameID: GameID): Game                         = new Game(gameID, size = 3, moves = Seq.empty)
  def fromQueryParams(queryParams: QueryParams): Task[Game] = for {
    gid   <-
      ZIO
        .fromOption(queryParams.getAll("gid").headOption)
        .flatMap(r => ZIO.fromTry(Try(UUID.fromString(r))))
        .orElseFail(new IllegalArgumentException("Invalid or missing gid"))
    size  <- ZIO
      .fromOption(queryParams.getAll("size").headOption)
      .flatMap(r => ZIO.fromTry(Try(Integer.parseInt(r))))
      .orElseFail(new IllegalArgumentException("Invalid or missing size"))
    moves <- ZIO
      .fromOption(queryParams.getAll("moves").headOption)
      .flatMap { r =>
        ZIO.attempt {
          r.split(";")
            .map(_.split(","))
            .map { case Array(symbol, x, y) => symbol.charAt(0) -> (x.toInt -> y.toInt) }
            .toSeq
        }
      }
      .orElseFail(new IllegalArgumentException("Invalid or missing moves"))
  } yield Game(
    gid,
    size,
    moves
  )

final class GameMaster private (endpointA: ServerEndpoint, endpointB: ServerEndpoint):
  private def mkGameID: Task[GameID]                                             = ZIO.attempt(UUID.randomUUID())
  private def mkMoveRequest(endpoint: ServerEndpoint, game: Game): Task[Request] = for
    url <- ZIO.fromEither(URL.decode(endpoint))
    request = Request.get(url).updatePath(_ => Path("/move")).setQueryParams(game.asQueryParams)
  yield request

  def play() = for
    client    <- ZIO.service[Client]
    gameID    <- mkGameID
    _         <- logInfo(s"Playing ${endpointA} vs ${endpointB} - gid: ${gameID}")
    emptyGame <- ZIO.succeed(Game.mkEmpty(gameID).withMove('X' -> (1, 1)).withMove('O' -> (0, 0)))
    r1        <- mkMoveRequest(endpointA, emptyGame)
    body      <- client.request(r1)
    _         <- printLine(body)
  yield ()

object GameMaster:
  def mkAndPlay(endpointA: ServerEndpoint, endpointB: ServerEndpoint): ZIO[Client & Scope, Throwable, Unit] =
    new GameMaster(endpointA, endpointB).play()

object GameServerApp extends ZIOAppDefault:
  override val bootstrap: ZLayer[ZIOAppArgs, Any, Any] =
    Runtime.removeDefaultLoggers >>> SLF4J.slf4j

  override def run: ZIO[Any, Any, Any] =
    for
      _ <- logInfo("🔥" * 3 + " tttm " + "🔥" * 3)
      ports @ (portA, portB) = 8761 -> 8768
      endpointA <- Promise.make[Nothing, ServerEndpoint]
      endpointB <- Promise.make[Nothing, ServerEndpoint]

      gsFib <- GameServer
        .mkAndRunWith(portA)(endpointA.succeed)
        .zipPar(GameServer.mkAndRunWith(portB)(endpointB.succeed))
        .fork

      gmFib <-
        endpointA.await
          .zipPar(endpointB.await)
          .flatMap((p1, p2) => GameMaster.mkAndPlay(p1, p2).provide(Client.default.and(Scope.default)))
          .fork

      _ <- gsFib.join
      _ <- gmFib.join
    yield ()
