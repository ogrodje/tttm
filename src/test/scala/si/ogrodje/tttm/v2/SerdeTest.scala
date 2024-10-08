package si.ogrodje.tttm.v2

import si.ogrodje.tttm.v2.GameTest.suite
import zio.*
import zio.logging.backend.SLF4J
import zio.test.*
import java.util.UUID

object SerdeTest extends ZIOSpecDefault:
  override val bootstrap: ZLayer[Any, Any, zio.test.TestEnvironment] =
    Runtime.removeDefaultLoggers >>> SLF4J.slf4j >>> testEnvironment

  def spec = suite("SerdeTest")(
    test("encoding empty game") {
      val gid  = UUID.randomUUID()
      val game = Game.make(gid, playerServerIDX = "x", playerServerIDO = "o")

      assertTrue(
        GameEncoder.encode(game).map == Map[String, Chunk[String]](
          "gid"     -> Chunk(gid.toString),
          "size"    -> Chunk("3"),
          "moves"   -> Chunk(""),
          "playing" -> Chunk("X")
        )
      )
    },
    test("encoding non-empty game") {
      val gid  = UUID.randomUUID()
      val game = Game
        .make(gid, playerServerIDX = "x", playerServerIDO = "o")
        .appendUnsafe(
          X -> (1, 1),
          O -> (0, 0),
          X -> (2, 2)
        )

      assertTrue(
        GameEncoder.encode(game).map == Map[String, Chunk[String]](
          "gid"     -> Chunk(gid.toString),
          "size"    -> Chunk("3"),
          "moves"   -> Chunk("X-1-1_O-0-0_X-2-2"),
          "playing" -> Chunk("X")
        )
      )
    },
    test("encoding-decoding") {
      val gid  = UUID.randomUUID()
      val game = Game
        .make(gid, playerServerIDX = "x", playerServerIDO = "o")
        .appendUnsafe(
          X -> (1, 1),
          O -> (0, 0),
          X -> (2, 2),
          O -> (0, 2)
        )

      for
        encodedGame  <- ZIO.succeed(GameEncoder.encode(game))
        decodedGame  <- GameDecoder.decode(encodedGame)
        encodedGame2 <- ZIO.succeed(GameEncoder.encode(decodedGame))

        withMove <- ZIO.succeed(
                      GameEncoder.encode(
                        decodedGame.appendUnsafe(X -> (0, 1)).withSwitchPlaying
                      )
                    )
      yield
        assertTrue(encodedGame == encodedGame2)
        assertTrue(withMove.map("moves") == Chunk("X-1-1_O-0-0_X-2-2_O-0-2_X-0-1"))
        assertTrue(withMove.map("playing") == Chunk("O"))
    }
  )
