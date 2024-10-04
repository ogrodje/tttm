package si.ogrodje.tttm.v2
import zio.{Task, ZIO}
import zio.http.{QueryParams, URL}
import ZIO.fromOption

trait QueryParamOps:
  enum ParamError extends Exception:
    case MissingParameter(key: String)  extends ParamError
    case DecodingError(message: String) extends ParamError

  import ParamError.*

  type Decoder[+T] = String => ZIO[Any, ParamError, T]
  given intDecoder: Decoder[Int] = s => ZIO.attempt(Integer.parseInt(s)).mapError(th => DecodingError(th.getMessage))
  given Decoder[URL]             = s => ZIO.fromEither(URL.decode(s)).mapError(th => DecodingError(th.getMessage))
  given Decoder[Long]            = s =>
    intDecoder(s).flatMap(s => ZIO.attempt(s.toLong)).mapError(th => DecodingError(th.getMessage))

  extension (queryPrams: QueryParams)
    def requiredAs[T](key: String)(using decoder: Decoder[T]): ZIO[Any, ParamError, T] =
      for
        rawValue <- fromOption(queryPrams.queryParam(key)).orElseFail(MissingParameter(key))
        value    <- decoder(rawValue)
      yield value

    def getAs[T](key: String)(using decoder: Decoder[T]): ZIO[Any, ParamError, Option[T]] =
      ZIO
        .succeed(queryPrams.queryParam(key))
        .flatMap {
          case Some(value) => decoder(value).map(Some(_))
          case _           => ZIO.none
        }

    def getAsWithDefault[T](key: String, default: T)(using decoder: Decoder[T]): ZIO[Any, ParamError, T] =
      getAs(key).map(_.getOrElse(default))
      
object QueryParamOps extends QueryParamOps
