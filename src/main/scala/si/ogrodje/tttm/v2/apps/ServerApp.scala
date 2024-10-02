package si.ogrodje.tttm.v2.apps

import si.ogrodje.tttm.v2.Match
import zio.*
import zio.http.*
import zio.ZIO.{logError, logInfo}
import zio.cli.*
import zio.http.{Client, URL}
import zio.logging.backend.SLF4J
import zio.*
import zio.stream.{Stream, ZStream}
import zio.Console.printLine
import zio.http.ChannelEvent.{ExceptionCaught, Read, UserEvent, UserEventTriggered}
import zio.http.WebSocketFrame.Text

enum ServerError extends RuntimeException:
  case MissingQueryParameter(name: String) extends ServerError

object ServerApp extends ZIOAppDefault:
  import ServerError.*
  override val bootstrap: ZLayer[ZIOAppArgs, Nothing, Any] =
    Runtime.removeDefaultLoggers >>> SLF4J.slf4j

  private def sandboxSocketApp(
    serverAUrl: URL,
    serverBUrl: URL,
    numberOfGames: Long
  ): WebSocketApp[zio.Scope & Client] =
    Handler.webSocket { channel =>
      channel.receiveAll {
        case UserEventTriggered(UserEvent.HandshakeComplete) =>
          for
            _      <-
              channel.send(
                Read(WebSocketFrame.text(s"Greetings lets play the match between ${serverAUrl} and ${serverBUrl}!"))
              )
            _      <- logInfo(s"Starting game between ${serverAUrl} and ${serverBUrl} w/ ${numberOfGames}")
            result <- Match.playGames(serverAUrl, serverBUrl, numberOfGames, concurrentProcesses = 3)
            _      <- logInfo("Match completed")

            _ <- channel.send(
                   Read(
                     WebSocketFrame.text(
                       result.toString()
                     )
                   )
                 )
            _ <- channel.shutdown
          yield ()
        case _                                               => ZIO.unit
      }
    }

  private def readURLFromQuery(queryParams: QueryParams, name: String): ZIO[Any, Throwable, URL] =
    ZIO.fromEither(queryParams.queryParam(name).toRight(MissingQueryParameter(name)).flatMap(URL.decode))

  private def readNumberOfGames(queryParams: QueryParams): ZIO[Any, Throwable, Int] =
    ZIO
      .succeed(queryParams.queryParam("number-of-games").getOrElse("10"))
      .flatMap(r => ZIO.attempt(Integer.parseInt(r)))

  val routes: Routes[Scope & Client, Nothing] =
    Routes(
      Method.GET / Root      -> handler(Response.text("Greetings at your service")),
      Method.GET / "greet"   -> handler { (req: Request) =>
        val name = req.queryParamToOrElse("name", "World")
        Response.text(s"Hello $name!")
      },
      Method.GET / "sandbox" -> handler { (req: Request) =>
        for
          serverAUrl    <- readURLFromQuery(req.queryParameters, "server-a")
          serverBUrl    <- readURLFromQuery(req.queryParameters, "server-b")
          numberOfGames <- readNumberOfGames(req.queryParameters)
          response      <- sandboxSocketApp(serverAUrl, serverBUrl, numberOfGames.toLong).toResponse
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
