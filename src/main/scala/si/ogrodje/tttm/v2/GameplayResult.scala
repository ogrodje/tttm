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

trait ServerMeasurements:
  def responseAverage: Double
  def responseMedian: Double
  def responseP99: Double
  def responseMax: Double
  def responseMin: Double

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

  def fromMoves(moves: Array[Move]): ServerResult =
    val measurements: Array[Double] = moves.map(_.duration.toMillis.toDouble)
    if measurements.isEmpty then throw new IllegalArgumentException("Moves array cannot be empty")
    val average                     = measurements.sum / measurements.length.toDouble

    // Sort array for median and p99 calculations
    val sorted = measurements.sorted

    // Median calculation
    val median = if sorted.length % 2 == 0 then
      val mid = sorted.length / 2
      (sorted(mid - 1) + sorted(mid)) / 2
    else sorted(sorted.length / 2)

    // P99 calculation
    val p99Index = math.ceil(measurements.length * 0.99).toInt - 1
    val p99      = sorted(p99Index)

    ServerResult(
      responseAverage = average,
      responseMedian = median,
      responseP99 = p99,
      responseMin = measurements.min,
      responseMax = measurements.max
    )
