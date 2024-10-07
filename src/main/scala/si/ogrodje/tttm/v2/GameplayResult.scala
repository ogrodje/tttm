package si.ogrodje.tttm.v2

import si.ogrodje.tttm.v2.Status.Won
import zio.Duration
import zio.json.*
import zio.json.internal.Write
import zio.prelude.NonEmptyList
import zio.schema.{DeriveSchema, Schema}

@jsonHintNames(SnakeCase)
final case class GameplayResult(
  @jsonField("duration_ms") duration: Duration,
  status: Status,
  @jsonField("winner") maybeWinner: Option[PlayerServerID] = None,
  @jsonField("server_a") serverA: ServerResult,
  @jsonField("server_b") serverB: ServerResult,
  @jsonExclude moves: List[Move] = List.empty
)
object GameplayResult:
  implicit val schema: Schema[GameplayResult]                  = DeriveSchema.gen
  given durationJsonEncoder: JsonEncoder[Duration]             = JsonEncoder[Double].contramap(_.toMillis)
  given gameplayResultJsonEncoder: JsonEncoder[GameplayResult] = DeriveJsonEncoder.gen[GameplayResult]

  def fromGame(servers: NonEmptyList[PlayerServer], duration: Duration, game: Game): GameplayResult =
    val List(serverA, serverB) = servers.toList

    GameplayResult(
      duration,
      game.status,
      maybeWinner = game.status match
        case Won(`X`) => Some(serverA.id)
        case Won(`O`) => Some(serverB.id)
        case _        => None,
      serverA = ServerResult.fromMoves(game.moves.filter(_.symbol == X)),
      serverB = ServerResult.fromMoves(game.moves.filter(_.symbol == O)),
      moves = game.moves.toList
    )

@jsonHintNames(SnakeCase)
final case class ServerResult(
  @jsonField("response_average_ms") responseAverage: Double,
  @jsonField("response_median_ms") responseMedian: Double,
  @jsonField("response_p99_ms") responseP99: Double,
  @jsonField("response_min_ms") responseMin: Double,
  @jsonField("response_max_ms") responseMax: Double
) extends ServerMeasurements

object ServerResult:
  implicit val schema: Schema[ServerResult]                = DeriveSchema.gen
  given serverResultJsonEncoder: JsonEncoder[ServerResult] = DeriveJsonEncoder.gen[ServerResult]
  def fromMoves(moves: Array[Move]): ServerResult          = ServerMeasurements.fromMoves(moves)(ServerResult.apply)
