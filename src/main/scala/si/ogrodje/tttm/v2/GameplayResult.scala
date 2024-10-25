package si.ogrodje.tttm.v2

import si.ogrodje.tttm.v2.Status.{CrashedBy, Won}
import zio.Duration
import zio.json.*
import zio.prelude.NonEmptyList
import zio.schema.{DeriveSchema, Schema}

@jsonHintNames(SnakeCase)
@jsonMemberNames(SnakeCase)
final case class GameplayResult(
  gid: GID,
  @jsonField("duration_ms") duration: Duration,
  status: Status,
  @jsonField("winner") maybeWinner: Option[PlayerServerID] = None,
  serverA: ServerResult,
  serverB: ServerResult,
  @jsonExclude moves: List[Move] = List.empty
)
object GameplayResult:
  implicit val schema: Schema[GameplayResult]                  = DeriveSchema.gen
  given durationJsonEncoder: JsonEncoder[Duration]             = JsonEncoder[Double].contramap(_.toMillis.toDouble)
  given gameplayResultJsonEncoder: JsonEncoder[GameplayResult] = DeriveJsonEncoder.gen[GameplayResult]

  def fromGame(
    servers: NonEmptyList[PlayerServer],
    duration: Duration,
    game: Game
  ): GameplayResult =
    val List(serverA, serverB) = servers.toList

    GameplayResult(
      gid = game.gid,
      duration,
      game.status,
      maybeWinner = game.status match
        case Won(`X`)          => Some(serverA.id)
        case Won(`O`)          => Some(serverB.id)
        case CrashedBy(`X`, _) => Some(serverB.id) // If server X crashes O wins.
        case CrashedBy(`O`, _) => Some(serverA.id) // If server O crashes X wins.
        case _                 => None,
      serverA = ServerResult.fromGame(game, X),
      serverB = ServerResult.fromGame(game, O),
      moves = game.moves.toList
    )

@jsonHintNames(SnakeCase)
@jsonMemberNames(SnakeCase)
final case class ServerResult(
  @jsonField("response_average_ms") responseAverage: Double = 0,
  @jsonField("response_median_ms") responseMedian: Double = 0,
  @jsonField("response_p99_ms") responseP99: Double = 0,
  @jsonField("response_min_ms") responseMin: Double = 0,
  @jsonField("response_max_ms") responseMax: Double = 0,
  numberOfMoves: Int = 0
) extends ServerMeasurements

object ServerResult:
  implicit val schema: Schema[ServerResult] = DeriveSchema.gen
  val empty: ServerResult                   = apply()

  given serverResultJsonEncoder: JsonEncoder[ServerResult] = DeriveJsonEncoder.gen[ServerResult]

  def fromGame(game: Game, symbol: Symbol): ServerResult =
    val serverResult = ServerMeasurements.fromMoves(game.moves.filter(_.symbol == symbol))(
      ServerResult.apply,
      ServerResult.empty
    )

    serverResult
