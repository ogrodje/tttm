package si.ogrodje.tttm.v2

import eu.timepit.refined.api.*
import eu.timepit.refined.boolean.Or
import eu.timepit.refined.generic.*
import zio.{IO, ZIO}

type Size = Int Refined Or[Equal[3], Or[Equal[5], Equal[7]]]

final case class WrongTypeError(message: String) extends IllegalArgumentException(message)

object Size:
  def safe(n: Int): Either[WrongTypeError, Size] =
    RefType.applyRef[Size](n).left.map(s => WrongTypeError(s"Wrong type with $s"))
  def safeZIO(n: Int): IO[Throwable, Size]       = ZIO.fromEither(safe(n))

  def unsafe(n: Int): Size = safe(n).toTry.get
  val default: Size        = unsafe(3)
