package si.ogrodje.tttm.v2.apps

import zio.*
import si.ogrodje.tttm.v2.QueryParamOps.*
import si.ogrodje.tttm.v2.*
import si.ogrodje.tttm.v2.server.*
import zio.ZIO.{logError, logInfo}
import zio.http.ChannelEvent.{Read, UserEvent, UserEventTriggered}
import zio.http.*
import zio.http.Header.{AccessControlAllowOrigin, Origin}
import zio.http.Middleware.{cors, CorsConfig}
import zio.json.*
import zio.logging.backend.SLF4J

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
  ): WebSocketApp[zio.Scope & Client] =
    Handler.webSocket { channel =>
      channel.receiveAll {
        case UserEventTriggered(UserEvent.HandshakeComplete) =>
          for
            (serverAUrl, serverBUrl) <-
              ZIO.succeed(
                serverA.serverEndpoint,
                serverB.serverEndpoint
              )
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
            _                        <-
              Match
                .playGames(
                  serverA,
                  serverB,
                  numberOfGames,
                  concurrentProcesses = 3,
                  size = size,
                  maybeGameplayReporter = Some(streamingReporter)
                )
                .foldZIO(
                  th =>
                    logError(th.getMessage) *>
                      channel.send(MatchError(s"Match error: ${th.getMessage}")),
                  matchResult => channel.send(MatchCompleted("Match has completed.", matchResult))
                )
                .race(
                  streamingReporter.listenWrite(rm => channel.send(GameMessage(rm.toMessage, rm)))
                )
            _                        <- channel.shutdown
          yield ()
        case _                                               => ZIO.unit
      }
    }

  private val routes: Routes[Scope & Client & PlayersConfig, Nothing] =
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
          size          <- ZIO.fromEither(Size.of(rawSize))
          response      <- sandboxSocketApp(serverA, serverB, size, numberOfGames).toResponse
        yield response
      }.tapErrorZIO(th => logError(s"Boom with ${th.getMessage}"))
        .orDie
        .catchAllDefect(th =>
          handler(
            Response.text(s"Sorry, request was not processed. Cause: ${th.getClass.getSimpleName} / ${th.getMessage}")
          )
        )
    ) @@ cors(corsConfig)

  def run = Server
    .serve(routes)
    .provide(
      Server.defaultWithPort(7777),
      Client.default.and(Scope.default),
      ZLayer.fromZIO(PlayersConfig.fromResources)
    )
