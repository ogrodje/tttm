package si.ogrodje.tttm.v2

import zio.*
import zio.logging.backend.SLF4J
import zio.test.*
import Status.*
import zio.test.TestAspect.*

object GameTest extends ZIOSpecDefault:
  override val bootstrap: ZLayer[Any, Any, zio.test.TestEnvironment] =
    Runtime.removeDefaultLoggers >>> SLF4J.slf4j >>> testEnvironment

  def spec = suite("GameTest")(
    test("empty game has size of 3") {
      val game = Game.empty
      assertTrue(game.size == 3)
      assertTrue(game.grid.length == 3 * 3)
      assertTrue(game.emptyPositions.length == 3 * 3)
      assertTrue(game.status == Pending)
    },
    test("adding moves") {
      val move = Move.of(X -> (1, 1))
      val g    = Game.empty.appendUnsafe(move)
      assertTrue(g.emptyPositions.length == (3 * 3) - 1)
      assertTrue(g.moves sameElements Array(move))
    },
    test("status") {
      assertTrue(Game.empty.status == Pending)
      assertTrue(Game.empty.appendUnsafe(X -> (1, 1)).status == Pending)

      val tideGame = Game.empty.appendUnsafe(
        X -> (0, 1),
        O -> (0, 0),
        X -> (0, 2),
        O -> (1, 2),
        X -> (1, 0),
        O -> (2, 0),
        X -> (1, 1),
        O -> (2, 1),
        X -> (2, 2)
      )
      assertTrue(tideGame.status == Tide)

      val wonGame = Game.empty.appendUnsafe(
        X -> (1, 1),
        O -> (0, 0),
        X -> (0, 2),
        O -> (0, 1),
        X -> (2, 0)
      )
      assertTrue(wonGame.status == Won(X))
    },
    test("status of tide") {
      val game = Game.empty.appendUnsafe(
        X -> (0, 0),
        O -> (1, 0),
        X -> (0, 1),
        O -> (1, 1),
        X -> (0, 2),
        O -> (1, 2)
      )
      assertTrue(game.status == Won(X))
    },
    test("appending") {
      assertTrue(Game.empty.append(X -> (1, 1), X -> (0, 0)).isLeft)
      assertTrue(Game.empty.append(X -> (1, 1), O -> (0, 0)).isRight)
      assertTrue(Game.empty.append(X -> (1, 1), O -> (1, 1)).isLeft)
      assertTrue(Game.empty.append(O -> (1, 1)).isLeft)
      assertTrue(Game.empty.append(X -> (1, 1)).isRight)
    }
  )
