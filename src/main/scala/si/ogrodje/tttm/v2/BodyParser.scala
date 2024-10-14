package si.ogrodje.tttm.v2
import zio.ZIO
import ZIO.fromEither

enum BodyParserError(val message: String) extends Throwable(message):
  case NoMoveFound                                     extends BodyParserError("No move payload found.")
  case NoErrorMessage                                  extends BodyParserError("No error message was present.")
  case CustomPlayerError(override val message: String) extends BodyParserError(message)

object BodyParser:
  import BodyParserError.*

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

  private def maybeError(body: String): Either[BodyParserError, String] =
    "Error:(.*)".r
      .findFirstMatchIn(body.take(255).trim)
      .map(_.group(1))
      .toRight(NoErrorMessage)

  def parse(raw: String): ZIO[Any, BodyParserError, Move] =
    val parsedMove  = fromEither(parseBody(raw))
    val parsedError = fromEither(maybeError(raw))

    parsedMove.orElse(parsedError).flatMap {
      case move: Move      => ZIO.succeed(move)
      case message: String => ZIO.fail(CustomPlayerError(message))
    }
