package si.ogrodje.tttm.v2.server

import si.ogrodje.tttm.v2.{MatchResult, ReporterMessage}
import zio.http.ChannelEvent.Read
import zio.http.*
import zio.json.*

enum ServerError extends RuntimeException:
  case MissingQueryParameter(name: String) extends ServerError

@jsonHintNames(SnakeCase)
@jsonDiscriminator("type")
enum Message(message: String):
  case Greet(
    message: String,
    @jsonField("server_a_url") serverAUrl: URL,
    @jsonField("server_b_url") serverBUrl: URL,
    size: Int,
    @jsonField("number_of_games") numberOfGames: Long
  ) extends Message(message)

  case MatchCompleted(
    message: String,
    matchResult: MatchResult
  ) extends Message(message)

  case MatchError(message: String) extends Message(message)
  case GameMessage(
    message: String,
    @jsonField("details") reporterMessage: ReporterMessage
  )                                extends Message(message)

object Message:
  private val jsonIndent: Option[Int]             = None
  given JsonEncoder[URL]                          = JsonEncoder[String].contramap(_.toString)
  given messageJsonEncoder: JsonEncoder[Message]  = DeriveJsonEncoder.gen
  given Conversion[Message, Read[WebSocketFrame]] = (m: Message) =>
    Read(WebSocketFrame.text(messageJsonEncoder.encodeJson(m, indent = jsonIndent).toString))
