package si.ogrodje.tttm.v2

import zio.{Task, ZIO}
import zio.http.URL

import java.nio.file.Path
import zio.json.*
import zio.json.yaml.*

final case class Player(
  name: String,
  author: String,
  authorURL: String,
  repositoryURL: String,
  endpointURL: String
  // tags: Set[String] = Set.empty
)

object Player:
  given playerJsonDecoder: JsonDecoder[Player] = DeriveJsonDecoder.gen[Player]

final case class PlayersConfig(players: List[Player])
object PlayersConfig:
  given playersJsonDecoder: JsonDecoder[PlayersConfig] = DeriveJsonDecoder.gen[PlayersConfig]

  def fromPath(path: Path) = ???
