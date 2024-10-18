package si.ogrodje.tttm.v2

import eu.timepit.refined.api.*
import eu.timepit.refined.boolean.Or
import eu.timepit.refined.generic.*

type Size = Int Refined Or[Equal[3], Or[Equal[5], Equal[7]]]

object Size:
  def safe(n: Int): Either[Throwable, Size] =
    RefType.applyRef[Size](n).left.map(s => new IllegalArgumentException(s"Wrong type with $s"))
  def unsafe(n: Int): Size                  = safe(n).toTry.get
  val default: Size                         = unsafe(3)
