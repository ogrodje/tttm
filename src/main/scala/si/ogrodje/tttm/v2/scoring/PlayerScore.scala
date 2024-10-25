package si.ogrodje.tttm.v2.scoring

import si.ogrodje.tttm.v2.PlayerServerID
import zio.json.*

final case class PlayerScore(id: PlayerServerID, score: Score)
object PlayerScore:
  given jsonEncoder: JsonEncoder[PlayerScore] = DeriveJsonEncoder.gen
  given jsonDecoder: JsonDecoder[PlayerScore] = DeriveJsonDecoder.gen
