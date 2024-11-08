package si.ogrodje.tttm.v2.persistance

import org.postgresql.util.PGInterval
import zio.*

object Ops:
  given durationToInterval: Conversion[Duration, PGInterval] = (zioDuration: Duration) =>
    val duration = zioDuration.asJava
    new PGInterval(
      0,
      0,
      0,
      0,
      0,
      duration.getSeconds.toDouble
    )
