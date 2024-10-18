package si.ogrodje.tttm.v2
import zio.prelude.NonEmptySet

type Sizes = NonEmptySet[Size]

object Sizes:
  val validSizes: Sizes = Sizes.unsafeOf(3, 5, 7)

  def safe(sizes: List[Int]): Either[String, NonEmptySet[Size]] = for
    rSet  <-
      sizes
        .map(Size.safe)
        .foldLeft(Right(Set.empty): Either[String, Set[Size]]) { (agg, c) =>
          c match
            case Left(th)     => Left(s"Invalid number detected - $th")
            case Right(value) => agg.map(_ ++ Set(value))
        }
    sizes <- NonEmptySet
               .fromSetOption[Size](rSet)
               .toRight("Could not create sizes set.")
  yield sizes

  def unsafe(sizes: List[Int]): Sizes =
    safe(sizes).left
      .map(m => new RuntimeException(s"Size creation failed with $m"))
      .toTry
      .get

  def unsafeOf(size: Int*): Sizes = unsafe(size.toList)
