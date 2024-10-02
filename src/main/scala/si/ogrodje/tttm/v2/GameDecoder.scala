package si.ogrodje.tttm.v2

import zio.{IO, Trace, Unsafe, ZIO}
import zio.ZIO.fromOption
import zio.http.QueryParams

import java.util.UUID

object GameDecoder:
  private val (moveSep, positionSep) = '_' -> "-"

  extension (queryParams: QueryParams)
    def readParam[A](key: String)(
      code: String => A
    )(implicit trace: Trace): ZIO[Any, Throwable, A] =
      ZIO
        .fromOption(queryParams.getAll(key).headOption)
        .flatMap(raw => ZIO.attempt(code(raw)))
        .orElseFail(new IllegalArgumentException(s"Missing query parameter $key"))

  def decode(queryParams: QueryParams): ZIO[Any, Throwable, Game] = for
    gid <- queryParams.readParam("gid")(UUID.fromString)

    maybePlaying <- queryParams.readParam("playing")(_.headOption)
    playing      <- fromOption(maybePlaying).orElseFail(new IllegalArgumentException("Missing playing symbol"))

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
