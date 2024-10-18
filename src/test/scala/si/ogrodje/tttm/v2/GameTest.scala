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
      assertTrue(game.size == Size.default)
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

      val tieGame = Game.empty.appendUnsafe(
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
      assertTrue(tieGame.isTie)

      val wonGame = Game.empty.appendUnsafe(
        X -> (1, 1),
        O -> (0, 0),
        X -> (0, 2),
        O -> (0, 1),
        X -> (2, 0)
      )
      assertTrue(wonGame.wonBy(X))
    },
    test("status of tie") {
      val game = Game.empty.appendUnsafe(
        X -> (0, 0),
        O -> (1, 0),
        X -> (0, 1),
        O -> (1, 1),
        X -> (0, 2),
        O -> (1, 2)
      )
      assertTrue(game.wonBy(X))
    },
    test("appending") {
      assertTrue(Game.empty.append(X -> (1, 1), X -> (0, 0)).isLeft)
      assertTrue(Game.empty.append(X -> (1, 1), O -> (0, 0)).isRight)
      assertTrue(Game.empty.append(X -> (1, 1), O -> (1, 1)).isLeft)
      assertTrue(Game.empty.append(O -> (1, 1)).isLeft)
      assertTrue(Game.empty.append(X -> (1, 1)).isRight)
    },
    test("size of 5") {
      val size = Size.safe(5).toTry.get
      assertTrue(
        Game
          .ofSize(size)
          .append(
            X -> (0, 0),
            O -> (0, 1),
            X -> (0, 2),
            O -> (0, 3),
            X -> (0, 4)
          )
          .isRight
      )
    },
    test("tie in 5x5") {
      val size = Size.safe(5).toTry.get
      val g    = Game.ofSize(size)
      assertTrue(g.size == size)
    },
    test("row win in 5x5") {
      val g = Game
        .ofSize(Size.unsafe(5))
        .appendUnsafe(
          X -> (0, 0),
          O -> (1, 0),
          X -> (0, 1),
          O -> (2, 1),
          X -> (0, 3),
          O -> (1, 2),
          X -> (1, 4),
          O -> (2, 3),
          X -> (0, 2)
        )
      assertTrue(g.wonBy(X))
    },
    test("col win of O in 5x5") {
      val g = Game
        .ofSize(Size.unsafe(5))
        .appendUnsafe(
          X -> (0, 0),
          O -> (1, 1),
          X -> (0, 2),
          O -> (2, 1),
          X -> (1, 3),
          O -> (3, 1),
          X -> (3, 4),
          O -> (4, 1)
        )
      assertTrue(g.wonBy(O))
    },
    test("size of 7") {
      assertTrue(Game.ofSize(Size.unsafe(7)).size == Size.unsafe(7))
    },
    test("won diagonal O in 7x7") {
      val g = Game
        .ofSize(Size.unsafe(7))
        .appendUnsafe(
          X -> (0, 0),
          O -> (1, 3),
          X -> (1, 5),
          O -> (2, 2),
          X -> (4, 6),
          O -> (0, 4),
          X -> (5, 3),
          O -> (3, 1)
        )

      assertTrue(g.status == Won(O))
    }
  )
