package si.ogrodje.tttm.v2

import zio.{IO, Trace, Unsafe, ZIO}
import zio.ZIO.fromOption
import zio.http.QueryParams

import java.util.UUID

enum DecoderError extends RuntimeException:
  case MissingQueryParameter(name: String) extends DecoderError
  case MissingPlayingSymbol                extends DecoderError

object GameDecoder:
  import DecoderError.*
  private val (moveSep, positionSep) = '_' -> "-"

  extension (queryParams: QueryParams)
    private def readParam[A](key: String)(
      code: String => A
    )(implicit trace: Trace): ZIO[Any, DecoderError, A] =
      ZIO
        .fromOption(queryParams.getAll(key).headOption)
        .flatMap(raw => ZIO.attempt(code(raw)))
        .orElseFail(MissingQueryParameter(key))

  def decode(queryParams: QueryParams): ZIO[Any, DecoderError, Game] = for
    gid          <- queryParams.readParam("gid")(UUID.fromString)
    maybePlaying <- queryParams.readParam("playing")(_.headOption)
    playing      <- fromOption(maybePlaying).orElseFail(MissingPlayingSymbol)

    moves <- fromOption(queryParams.getAll("moves").headOption).flatMap { r =>
      ZIO.attempt(
        r.split(moveSep)
          .map(_.split(positionSep, 3))
          .map {
            case Array(symbol, x, y) if validSymbols.contains(symbol.charAt(0)) =>
              symbol.charAt(0) -> (x.toInt -> y.toInt)
          }
      )
    }.orElse(ZIO.succeed(Array.empty[Move]))
  yield Game.make(
    gid,
    playing,
    size = 3,
    moves
  )
