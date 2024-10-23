package si.ogrodje.tttm.v2

import si.ogrodje.tttm.v2.ServerMeasurements.*

import scala.util.Try

trait ServerMeasurements:
  def responseAverage: Average
  def responseMedian: Median
  def responseP99: P99
  def responseMax: Max
  def responseMin: Min
  def numberOfMoves: NumberOfMoves

object ServerMeasurements:
  private type BaseValue     = Double
  private type Average       = BaseValue
  private type Median        = BaseValue
  private type P99           = BaseValue
  private type Min           = BaseValue
  private type Max           = BaseValue
  private type NumberOfMoves = Int
  private type Measurements  = (
    Average,
    Median,
    P99,
    Min,
    Max,
    NumberOfMoves
  )

  private def rawFromMoves[O <: ServerMeasurements](moves: Array[Move])(
    measurementsF: Measurements => O
  ): O =
    val measurements = moves.map(_.duration.toMillis.toDouble)
    if measurements.isEmpty then throw new IllegalArgumentException("Moves array cannot be empty")
    val average      = measurements.sum / measurements.length.toDouble

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

    measurementsF(
      (average, median, p99, measurements.min, measurements.max, moves.length)
    )

  def fromMoves[O <: ServerMeasurements](moves: Array[Move])(
    measurementsF: Measurements => O,
    default: O
  ): O =
    Try(rawFromMoves(moves)(measurementsF)).getOrElse(default)
