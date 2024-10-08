package si.ogrodje.tttm.v2.apps

import si.ogrodje.tttm.v2.{
  ExternalPlayerServer,
  Match,
  MatchResult,
  PlayerServer,
  QueryParamOps,
  ReporterMessage,
  StreamingReporter
}
import QueryParamOps.*
import zio.*
import zio.http.*
import zio.ZIO.{logError, logInfo}
import zio.cli.*
import zio.http.{Client, URL}
import zio.logging.backend.SLF4J
import zio.*
import zio.stream.*
import zio.Console.printLine
import zio.http.ChannelEvent.{ExceptionCaught, Read, UserEvent, UserEventTriggered}
import zio.http.WebSocketFrame.Text
import zio.json.*

enum ServerError extends RuntimeException:
  case MissingQueryParameter(name: String) extends ServerError

@jsonHintNames(SnakeCase)
@jsonDiscriminator("type")
enum Message(message: String):
  case Greet(
    message: String,
    @jsonField("server_a_url") serverAUrl: URL,
    @jsonField("server_b_url") serverBUrl: URL,
    size: Int,
    @jsonField("number_of_games") numberOfGames: Long
  ) extends Message(message)

  case MatchCompleted(
    message: String,
    matchResult: MatchResult
  ) extends Message(message)

  case MatchError(message: String) extends Message(message)
  case GameMessage(
    message: String,
    @jsonField("details") reporterMessage: ReporterMessage
  )                                extends Message(message)

object Message:
  private val jsonIndent: Option[Int]             = None
  given JsonEncoder[URL]                          = JsonEncoder[String].contramap(_.toString)
  given messageJsonEncoder: JsonEncoder[Message]  = DeriveJsonEncoder.gen
  given Conversion[Message, Read[WebSocketFrame]] = (m: Message) =>
    Read(WebSocketFrame.text(messageJsonEncoder.encodeJson(m, indent = jsonIndent).toString))

object ServerApp extends ZIOAppDefault:
  import ServerError.*
  import Message.*
  override val bootstrap: ZLayer[ZIOAppArgs, Nothing, Any] =
    Runtime.removeDefaultLoggers >>> SLF4J.slf4j

  private def sandboxSocketApp(
    serverA: PlayerServer,
    serverB: PlayerServer,
    size: Int,
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
                  size,
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

  private val routes: Routes[Scope & Client, Nothing] =
    Routes(
      Method.GET / Root      -> handler(Response.text("Hello.")),
      Method.GET / "sandbox" -> handler { (req: Request) =>
        for
          serverAUrl    <- req.queryParameters.requiredAs[URL]("server-a")
          serverBUrl    <- req.queryParameters.requiredAs[URL]("server-b")
          serverA        = ExternalPlayerServer.unsafeFromURL(serverAUrl)
          serverB        = ExternalPlayerServer.unsafeFromURL(serverBUrl)
          numberOfGames <- req.queryParameters.getAsWithDefault[Long]("number-of-games", 10)
          size          <- req.queryParameters.getAsWithDefault[Int]("size", 3)
          response      <- sandboxSocketApp(serverA, serverB, size, numberOfGames).toResponse
        yield response
      }.tapErrorZIO(th => logError(s"Boom with ${th.getMessage}"))
        .orDie
        .catchAllDefect(th =>
          handler(
            Response.text(s"Sorry, request was not processed. Cause: ${th.getClass.getSimpleName} / ${th.getMessage}")
          )
        )
    )

  def run = Server
    .serve(routes)
    .provide(Server.defaultWithPort(7777), Client.default.and(Scope.default))
