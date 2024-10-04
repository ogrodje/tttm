package si.ogrodje.tttm.v2

import zio.{IO, Task, Trace, Unsafe, ZIO}
import zio.ZIO.fromOption
import zio.http.QueryParams

import java.util.UUID

enum DecoderError extends RuntimeException:
  case MissingQueryParameter(name: String) extends DecoderError
  case MissingPlayingSymbol                extends DecoderError

object GameDecoder:
  import DecoderError.*
  private val (positionSep, moveSep) = ("-", '_')

  extension (queryParams: QueryParams)
    private def readParam[A](key: String)(
      code: String => A
    )(implicit trace: Trace): ZIO[Any, DecoderError, A] =
      ZIO
        .fromOption(queryParams.getAll(key).headOption)
        .flatMap(raw => ZIO.attempt(code(raw)))
        .orElseFail(MissingQueryParameter(key))

  private def splitMoves(raw: String): Task[Array[Move]] = ZIO.attempt(
    raw
      .split(moveSep)
      .map(_.split(positionSep, 3))
      .map {
        case Array(symbol, x, y) if validSymbols.contains(symbol.charAt(0)) =>
          Move(
            symbol = symbol.charAt(0),
            position = x.toInt -> y.toInt
          )
      }
  )

  def decode(queryParams: QueryParams): ZIO[Any, DecoderError, Game] = for
    gid          <- queryParams.readParam("gid")(UUID.fromString)
    maybePlaying <- queryParams.readParam("playing")(_.headOption)
    playing      <- fromOption(maybePlaying).orElseFail(MissingPlayingSymbol)
    moves        <-
      fromOption(queryParams.getAll("moves").headOption)
        .flatMap(splitMoves)
        .orElse(ZIO.succeed(Array.empty[Move]))
  yield Game.make(
    gid,
    playerServerIDX = "pid-x", // not needed for client
    playerServerIDO = "pid-o", // not needed for client
    playing,
    size = 3,
    moves
  )
