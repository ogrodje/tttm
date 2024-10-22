package si.ogrodje.tttm.v2

import zio.ZIO.fromOption
import zio.http.QueryParams
import zio.{Task, UIO, ZIO}

import java.util.UUID

enum DecoderError(val message: String) extends RuntimeException(message):
  case MissingQueryParameter(name: String)             extends DecoderError(s"Problem decoding query parameter $name")
  case ProblemDecodingParameterToType(details: String) extends DecoderError(s"Query type error: $details")
  case MissingPlayingSymbol                            extends DecoderError(s"Playing symbol is missing")

object GameDecoder:
  import DecoderError.*
  import QueryParamOps.*

  private val (positionSep, moveSep) = ("-", '_')

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

  private def processMoves(queryParams: QueryParams): UIO[Array[Move]] =
    fromOption(queryParams.getAll("moves").headOption)
      .flatMap(splitMoves)
      .orElse(ZIO.succeed(Array.empty[Move]))

  def decode(queryParams: QueryParams): ZIO[Any, DecoderError, Game] = for
    gid     <- queryParams.requiredAs[UUID]("gid").mapError(paramError => MissingQueryParameter("gid"))
    playing <- queryParams.requiredAs[Symbol]("playing").orElseFail(MissingPlayingSymbol)
    size    <- queryParams.requiredAs[Size]("size").mapError(paramError => MissingQueryParameter("size"))
    moves   <- processMoves(queryParams)
  yield Game.make(
    gid,
    playerServerIDX = "pid-x", // not needed for client
    playerServerIDO = "pid-o", // not needed for client
    playing,
    size,
    moves
  )
