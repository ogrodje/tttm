package si.ogrodje.tttm.v2

import si.ogrodje.tttm.v2.Status.*
import zio.Console.printLine
import zio.ZIO.logInfo
import zio.http.*
import zio.{Task, ZIO}

import scala.util.Random

final class SimplePlayerServer private (port: Int) extends PlayerServer:

  // Pick random empty field. No strategy.
  private val computeMove: Game => Either[String, (Symbol, Position)] = game =>
    game.status match
      case Pending =>
        val randomPosition: Position = Random.shuffle(game.emptyPositions).head
        Right(game.playing -> randomPosition)
      case _       => Left("Can't do anything.")

  private def handleMove(request: Request): Task[Response] = for
    game    <- GameDecoder.decode(request.queryParameters)
    _       <- logInfo(s"Playing: ${game.playing}, size: ${game.size}, status: ${game.status}, gid: ${game.gid}")
    response = computeMove(game) match
                 case Left(err)               => Response.text(s"Error:$err")
                 case Right((symbol, (x, y))) => Response.text(s"Move:$symbol-$x-$y")
  yield response

  private val routes = Routes(
    Method.GET / Root   -> handler(Response.text("Nothing here. I'm just a server playing the game.")),
    Method.GET / "move" -> handler(handleMove).tapErrorZIO(th => printLine(s"Boom - ${th}"))
  ).transform(
    _.catchAllCause(cause =>
      handler(
        ZIO
          .logErrorCause(cause)
          .as(Response.internalServerError(s"Error: $cause"))
      )
    )
  ).sandbox

  override def serverEndpoint: ServerEndpoint = s"http://localhost:$port"

  def run: Task[Nothing] =
    logInfo(s"Booting server on port $port") *>
      Server.serve(routes).provide(Server.defaultWithPort(port))

object SimplePlayerServer:
  def make(port: Int) = new SimplePlayerServer(port)
