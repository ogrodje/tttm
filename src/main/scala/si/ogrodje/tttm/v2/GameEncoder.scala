package si.ogrodje.tttm.v2

import zio.Chunk
import zio.http.QueryParams

object GameEncoder:
  private def encodedMoves(game: Game): String =
    game.listMoves.map { case (symbol, (x, y)) => s"$symbol-$x-$y" }.mkString("_")

  def encode(game: Game): QueryParams =
    QueryParams.apply(
      Map(
        "gid"     -> Chunk(game.gid.toString),
        "size"    -> Chunk(game.size.toString),
        "playing" -> Chunk(game.playing.toString),
        "moves"   -> Chunk(encodedMoves(game))
      )
    )
