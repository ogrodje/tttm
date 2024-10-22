package si.ogrodje.tttm.v2
import zio.ZIO.{attempt, fail, from, fromEither, fromOption, fromTry, succeed}
import zio.http.{QueryParams, URL}
import zio.{IO, ZIO}

import java.util.UUID
import scala.util.Try

trait QueryParamOps:
  enum ParamError extends Exception:
    case MissingParameter(key: String)  extends ParamError
    case DecodingError(message: String) extends ParamError

  import ParamError.*

  private type Decoder[+T] = String => IO[ParamError, T]
  given intDecoder: Decoder[Int] = s => attempt(Integer.parseInt(s)).mapError(th => DecodingError(th.getMessage))
  given Decoder[URL]             = s => fromEither(URL.decode(s)).mapError(th => DecodingError(th.getMessage))
  given Decoder[Long]            = s => intDecoder(s).flatMap(s => attempt(s.toLong)).mapError(th => DecodingError(th.getMessage))
  given Decoder[UUID]            = s => fromTry(Try(UUID.fromString(s))).mapError(th => DecodingError(th.getMessage))

  given Decoder[Symbol] = s =>
    fromTry(Try(s.charAt(0)))
      .map(s => s -> validSymbols.contains(s))
      .flatMap {
        case (s, true) => succeed(s)
        case _         => fail(DecodingError("Invalid or missing symbol."))
      }
      .orElseFail(DecodingError("Problem decoding symbol"))

  given Decoder[Size] =
    s =>
      intDecoder(s)
        .flatMap(sInt => from(Size.safe(sInt)))
        .mapError(th => DecodingError(th.getMessage))

  given [T](using decoder: Decoder[T]): Decoder[Option[T]] = s =>
    if s.isEmpty then ZIO.none else decoder(s).map(Some(_))

  extension (queryPrams: QueryParams)
    def requiredAs[T](key: String)(using decoder: Decoder[T]): ZIO[Any, ParamError, T] = for
      rawValue <- fromOption(queryPrams.queryParam(key)).orElseFail(MissingParameter(key))
      value    <- decoder(rawValue)
    yield value

    def getAs[T](key: String)(using decoder: Decoder[T]): ZIO[Any, ParamError, Option[T]] =
      succeed(queryPrams.queryParam(key)).flatMap {
        case Some(value) => decoder(value).map(Some(_))
        case _           => ZIO.none
      }

    def getAsWithDefault[T](key: String, default: T)(using decoder: Decoder[T]): ZIO[Any, ParamError, T] =
      getAs(key).map(_.getOrElse(default))

object QueryParamOps extends QueryParamOps
