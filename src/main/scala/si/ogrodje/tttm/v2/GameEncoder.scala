package si.ogrodje.tttm.v2

import zio.{Chunk, UIO, ZIO}
import zio.http.QueryParams

object GameEncoder:
  private def encodedMoves(game: Game): String =
    game.listMoves.map { case Move(symbol, (x, y), _, _) => s"$symbol-$x-$y" }.mkString("_")

  def encode(game: Game): QueryParams =
    QueryParams.apply(
      Map(
        "gid"     -> Chunk(game.gid.toString),
        "size"    -> Chunk(game.size.toString),
        "playing" -> Chunk(game.playing.toString),
        "moves"   -> Chunk(encodedMoves(game))
      )
    )

  def encodeZIO(game: Game): UIO[QueryParams] = ZIO.succeed(encode(game))
