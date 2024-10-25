package si.ogrodje.tttm.v2.apps

import doobie.implicits.*
import doobie.*
import zio.interop.catz.*
import si.ogrodje.tttm.v2.*
import si.ogrodje.tttm.v2.QueryParamOps.*
import si.ogrodje.tttm.v2.persistance.{DB, *}
import si.ogrodje.tttm.v2.persistance.DB.TransactorTask
import si.ogrodje.tttm.v2.server.*
import zio.*
import zio.ZIO.{logError, logInfo, provideLayer}
import zio.http.*
import zio.http.ChannelEvent.{UserEvent, UserEventTriggered}
import zio.http.Middleware.{cors, CorsConfig}
import zio.logging.backend.SLF4J
import eu.timepit.refined.auto.*

import java.nio.file.Path
import java.util.UUID

object ServerApp extends ZIOAppDefault:
  import Message.*
  override val bootstrap: ZLayer[ZIOAppArgs, Nothing, Any] =
    Runtime.removeDefaultLoggers >>> SLF4J.slf4j

  private val corsConfig: CorsConfig =
    CorsConfig(
      allowedOrigin = _ => Some(Header.AccessControlAllowOrigin.All),
      allowedMethods = Header.AccessControlAllowMethods.All,
      allowedHeaders = Header.AccessControlAllowHeaders.All
    )

  private def sandboxSocketApp(
    serverA: PlayerServer,
    serverB: PlayerServer,
    size: Size,
    numberOfGames: Long
  ): WebSocketApp[zio.Scope & Client] = Handler.webSocket { channel =>
    channel.receiveAll {
      case UserEventTriggered(UserEvent.HandshakeComplete) =>
        for
          (serverAUrl, serverBUrl) <- ZIO.succeed(serverA.serverEndpoint -> serverB.serverEndpoint)
          streamingReporterQueue   <- StreamingReporter.queue
          streamingReporter        <- StreamingReporter.fromQueue(streamingReporterQueue)
          _                        <-
            logInfo(s"Starting game between $serverAUrl and $serverBUrl w/ $numberOfGames, size: $size")
          _                        <-
            channel.send(
              Greet(
                s"Hello. Let's play a match between $serverA and $serverBUrl, size: $size, number of games: $numberOfGames",
                serverAUrl,
                serverBUrl,
                size.value,
                numberOfGames
              )
            )

          _ <-
            Match
              .mk(serverA, serverB, numberOfGames, size)
              .playGames(concurrentProcesses = 3, maybeGameplayReporter = Some(streamingReporter))
              .foldZIO(
                th => logError(th.getMessage) *> channel.send(MatchError(s"Match error: ${th.getMessage}")),
                matchResult => channel.send(MatchCompleted("Match has completed.", matchResult))
              )
              .race(
                streamingReporter.listenWrite(rm => channel.send(GameMessage(rm.toMessage, rm)))
              )
          _ <- channel.shutdown
        yield ()

      case _ => ZIO.unit
    }
  }

  private val routes: Routes[Scope & Client & PlayersConfig & TransactorTask, Nothing] =
    Routes(
      Method.GET / Root      -> handler(Response.text("Hello.")),
      Method.GET / "players" -> handler { (req: Request) =>
        for playersConfig <- ZIO.service[PlayersConfig]
        yield Response.json(PlayersConfig.playersJsonEncoder.encodeJson(playersConfig))
      },
      Method.GET / "sandbox" -> handler { (req: Request) =>
        for
          serverAUrl    <- req.queryParameters.requiredAs[URL]("server-a")
          serverBUrl    <- req.queryParameters.requiredAs[URL]("server-b")
          serverA        = ExternalPlayerServer.unsafeFromURL(serverAUrl)
          serverB        = ExternalPlayerServer.unsafeFromURL(serverBUrl)
          numberOfGames <- req.queryParameters.getAsWithDefault[Long]("number-of-games", 10)
          rawSize       <- req.queryParameters.getAsWithDefault[Int]("size", 3)
          size          <- Size.safeZIO(rawSize)
          response      <- sandboxSocketApp(serverA, serverB, size, numberOfGames).toResponse
        yield response
      }.tapErrorZIO(th => logError(s"Boom with ${th.getMessage}"))
        .orDie
        .catchAllDefect(th =>
          handler(
            Response.text(s"Sorry, request was not processed. Cause: ${th.getClass.getSimpleName} / ${th.getMessage}")
          )
        ),

      // Tournaments
      Method.GET / "tournament" / "latest"     ->
        handler(TournamentsView.latestTournament(_)).orDie.catchAllDefect(errorHandler),
      Method.GET / "tournament" / string("id") ->
        handler { (id: String, req: Request) =>
          TournamentsView.read(UUID.fromString(id), req)
        }.orDie.catchAllDefect(errorHandler)
    ) @@ cors(corsConfig)

  private def errorHandler(th: Throwable) =
    handler(
      Response.text(s"Sorry, request was not processed. Cause: ${th.getClass.getSimpleName} / ${th.getMessage}")
    )

  def run: Task[Nothing] = runWithPort().orDieWith(any => new RuntimeException("Boom."))

  def runWithPort(port: Int = 7777): Task[Nothing] = (for
    _        <- logInfo(s"Booting server on port $port")
    dbConfig <- ZIO.service[DBConfiguration]
    _        <- DB.loadMigrate
    server   <-
      Server
        .serve(routes)
        .provide(
          Server.defaultWithPort(port),
          RichClient.live.and(Scope.default),
          ZLayer.fromZIO(PlayersConfig.fromFile(Path.of("players.yml"))),
          ZLayer.fromZIO(DB.transactor.make(dbConfig))
        )
  yield server).provide(DBConfiguration.live)
