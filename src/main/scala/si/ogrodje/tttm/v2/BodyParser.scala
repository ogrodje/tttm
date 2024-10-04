package si.ogrodje.tttm.v2
import zio.ZIO
import ZIO.fromEither

enum BodyParserError extends Exception:
  case NoMoveFound                        extends BodyParserError
  case NoErrorMessage                     extends BodyParserError
  case CustomPlayerError(message: String) extends BodyParserError

object BodyParser:
  import BodyParserError.*

  type ErrorMessage = String

  private def parseBody(body: String): Either[BodyParserError, Move] =
    "Move:([XO])-(\\d)-(\\d)".r
      .findFirstMatchIn(body)
      .map { m =>
        Move(
          symbol = m.group(1).charAt(0),
          position = (m.group(2).toInt, m.group(3).toInt)
        )
      }
      .toRight(NoMoveFound)

  private def maybeError(body: String): Either[BodyParserError, ErrorMessage] =
    "Error:(.*)".r
      .findFirstMatchIn(body.take(255).trim)
      .map(_.group(1))
      .toRight(NoErrorMessage)

  def parse(raw: String): ZIO[Any, BodyParserError, Move] =
    val parsedMove  = fromEither(parseBody(raw))
    val parsedError = fromEither(maybeError(raw))

    parsedMove.orElse(parsedError).flatMap {
      case move: Move            => ZIO.succeed(move)
      case message: ErrorMessage => ZIO.fail(CustomPlayerError(message))
    }
