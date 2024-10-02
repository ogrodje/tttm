package si.ogrodje.tttm.v2

import zio.*
import zio.test.*
import zio.stream.ZStream
import zio.Console.printLine
import zio.logging.backend.SLF4J
import java.util.concurrent.TimeUnit

object ElloStreamTest extends ZIOSpecDefault:
  override val bootstrap: ZLayer[Any, Any, zio.test.TestEnvironment] =
    Runtime.removeDefaultLoggers >>> SLF4J.slf4j >>> testEnvironment

  def spec = suite("ElloStreamTest")(test("Just have some fun") {

    val numbers =
      ZStream
        .fromIterable(1 to 10)
        .schedule(Schedule.fixed(Duration(200, TimeUnit.MILLISECONDS)))

    val characters =
      ZStream
        .fromIterable(1 to 10)
        .map(e => (('a' - 1) + e).toChar)
        .schedule(Schedule.fixed(Duration(100, TimeUnit.MILLISECONDS)))

    val out = numbers
      .zipLatest(characters)
      .tap((i, c) => printLine(s"Hello ${i} - ${c}"))
      .runDrain

    assertZIO(numbers.runDrain.as(true))(
      Assertion.isTrue
    )
  })
