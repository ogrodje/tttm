package si.ogrodje.tttm.v2

import si.ogrodje.tttm.v2.SerdeTest.{suite, test}
import zio.*
import zio.test.*
import zio.test.TestAspect.*
import zio.logging.backend.SLF4J

import scala.reflect.ClassTag

object MathSampleTest extends ZIOSpecDefault:
  override val bootstrap: ZLayer[Any, Any, zio.test.TestEnvironment] =
    Runtime.removeDefaultLoggers >>> SLF4J.slf4j >>> testEnvironment

  def spec = suite("SerdeTest")(
    test("encoding empty game") {

      // Sum
      val r = (0 to 10).reduce((a, n) => a + 4 * n)
      val x = (0 to 20).map(n => n * 3).sum

      println(r)
      println(x)

      // Product
      val r2 = (1 to 20).map(n => n * 5).product
      println(r2)

      assertTrue(1 == 1)
    },
    test("matrix") {
      type Matrix[N] = Array[Array[N]]
      val ones: Matrix[Int] = Array.tabulate(3, 3)((_, _) => 1)

      extension [N](m: Matrix[N])
        def mapElement[B: ClassTag](f: N => B): Matrix[B] = m.map(_.map(f))
        def +(b: Matrix[N]): Matrix[N]                    = b

        def show = for (r <- 0 to m.size; c <- 0 to m.size) yield (r + c)

      val zero = ones.mapElement(_ => 0)
      val twos = ones.mapElement(_ => 2)

      val sum = ones + zero + twos
      println(sum)
      println(sum.show)

      assertTrue(2 == 2)
    }
  )
