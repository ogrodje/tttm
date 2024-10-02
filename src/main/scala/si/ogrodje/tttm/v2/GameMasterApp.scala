package si.ogrodje.tttm.v2

import zio.ZIO.logInfo
import zio.http.*
import zio.logging.backend.SLF4J
import zio.{durationInt, Runtime, Scope, Task, ZIO, ZIOAppArgs, ZIOAppDefault, ZLayer}

type ServerEndpoint = String
trait GameServer:
  def serverEndpoint: ServerEndpoint
  val show: String = serverEndpoint

final case class ExternalServer(serverEndpoint: ServerEndpoint) extends GameServer

object GameMasterApp extends ZIOAppDefault:
  override val bootstrap: ZLayer[ZIOAppArgs, Nothing, Any] =
    Runtime.removeDefaultLoggers >>> SLF4J.slf4j

  override def run: ZIO[Any, Any, Any] = for {
    _ <- logInfo("Booting server,...")
    // (serverA, serverB) = SimpleGameServer.make(6776) -> SimpleGameServer.make(6777)
    // serverFib <- ZIO.raceAll(serverA.run, serverB.run :: Nil).fork

    serverA = SimpleGameServer.make(6776)
    serverB = ExternalServer("http://127.0.0.1:5000")
    serverFib <- serverA.run.fork

    /*
    gameplayFib <- (ZIO.sleep(3.seconds) *> Gameplay
      .make(serverA, serverB)
      .play()
      .provide(Client.default.and(Scope.default))).fork
    _ <- gameplayFib.join
     */

    gameplayFib <- (ZIO.sleep(3.seconds) *>
      Gameplay2
        .make(serverA, serverB)
        .play
        .provide(Client.default.and(Scope.default))).fork

    _ <- gameplayFib.join
    _ <- serverFib.join
  } yield ()
