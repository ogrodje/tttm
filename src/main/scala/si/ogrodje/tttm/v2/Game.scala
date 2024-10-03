package si.ogrodje.tttm.v2

import zio.json.*
import zio.{Duration, Task, ZIO}
import zio.prelude.{NonEmptySet, Validation}

import java.util.UUID
import scala.annotation.targetName

type Symbol = Char
val X: Symbol                         = 'X'
val O: Symbol                         = 'O'
val validSymbols: NonEmptySet[Symbol] = NonEmptySet(X, O)
val oppositeSymbol: Symbol => Symbol  = symbol => if symbol == X then O else X
val defaultPlaying: Symbol            = X

type GID      = UUID
type Position = (Int, Int)
final case class Move(symbol: Symbol, position: Position, duration: Duration = Duration.Zero)
object Move:
  def of(symbol: Symbol, position: Position): Move = Move(symbol, position)
  def of(tpl: (Symbol, Position)): Move            = Move(tpl._1, tpl._2)

type Moves = Array[Move]
type Size  = Int
object Size:
  val default: Size = 3

@jsonDiscriminator("type")
enum Status:
  case Tie
  case Pending
  case Won(symbol: Symbol)
object Status:
  given eventJsonEncoder: JsonEncoder[Status] = DeriveJsonEncoder.gen[Status]

final case class Game private (
  gid: GID,
  playing: Symbol = defaultPlaying,
  moves: Moves = Array.empty[Move],
  size: Size = 3
):
  import Status.*
  private def symbolAt: Position => Option[Symbol] = (x, y) =>
    moves.find { case Move(_, (px, py), _) => px == x && py == y }.map(_._1)

  private def listOfSymbols: Symbol => List[Option[Symbol]] =
    symbol => (0 until size).toList.map(_ => Some(symbol))

  def grid: Array[(Option[Symbol], Position)] = for
    x <- (0 until size).toArray
    y <- 0 until size
  yield symbolAt(x, y) -> (x, y)

  def listMoves: Array[Move] = moves

  private def wonRows(symbol: Symbol): Boolean =
    grid.grouped(size).map(_.map(_._1)).exists(_.sameElements(listOfSymbols(symbol)))

  private def wonColumns(symbol: Symbol): Boolean =
    (0 until size).map(r => (0 until size).map(symbolAt(_, r))).contains(listOfSymbols(symbol))

  private def wonDiagonals(symbol: Symbol): Boolean =
    val (left, right) = (
      (0 until size).zip(0 until size).map(symbolAt) == listOfSymbols(symbol),
      (0 until size).zip(size - 1 to 0 by -1).map(symbolAt) == listOfSymbols(symbol)
    )
    left || right

  private def hasWon(symbol: Symbol): Boolean =
    Seq(wonRows, wonColumns, wonDiagonals).exists(f => f(symbol))

  private def isFull: Boolean = moves.length == size * size

  def emptyPositions: Array[Position] =
    grid.collect { case (None, position) => position }

  def status: Status =
    hasWon(X) -> hasWon(O) match
      case true -> false => Won(X)
      case false -> true => Won(O)
      case true -> true  => Won(X)
      case _ if isFull   => Tie
      case _             => Pending

  def show: String =
    grid
      .grouped(size)
      .map(_.map { case (Some(symbol), _) => symbol; case _ => " " }.mkString(","))
      .mkString("\n")

  def withSwitchPlaying: Game = this.copy(playing = oppositeSymbol(playing))

  private def nonEmptyMoves(moves: Seq[Move]): Validation[Throwable, Seq[Move]] =
    Validation.fromPredicateWith(new IllegalArgumentException("At least one move is needed."))(moves)(_.nonEmpty)

  private def validateSymbols(moves: Seq[Move]): Validation[Throwable, Seq[Move]] =
    Validation.fromPredicateWith(new IllegalArgumentException("Only X or O symbols are allowed"))(moves)(
      _.forall { case Move(symbol, _, _) => validSymbols.contains(symbol) }
    )

  private def validSequence(moves: Seq[Move]): Validation[Throwable, Seq[Move]] =
    if moves.isEmpty then Validation.succeed(moves)
    else
      Validation
        .fromPredicate(moves)(_.sliding(2).forall {
          case Seq(Move(`X`, _, _), Move(`O`, _, _)) => true
          case Seq(Move(`O`, _, _), Move(`X`, _, _)) => true
          case Seq(_, _)                             => false
          case _                                     => true
        })
        .mapError(_ => new RuntimeException(s"Invalid sequence of symbols: ${moves.map(_._1).mkString(", ")}"))

  private def validateFirstMove(game: Game, moves: Seq[Move]): Validation[Throwable, Seq[Move]] =
    Validation.fromPredicateWith(new RuntimeException("First move needs to be X"))((game.moves ++ moves).toSeq)(
      _.headOption.exists { case Move(symbol, _, _) => symbol == X }
    )

  private def validateAppend(game: Game, moves: Move*): Validation[Throwable, Seq[Move]] =
    nonEmptyMoves(moves.toSeq)
      .flatMap(validateSymbols)
      .flatMap(validSequence)
      .flatMap(moves => validateFirstMove(game, moves).as(moves))

  def appendUnsafe(moves: Move*): Game = append(moves*).toTry.get

  @targetName("appendUnsafeFromTuples")
  def appendUnsafe(moves: (Symbol, Position)*): Game =
    append(moves.map((symbol, position) => Move.of(symbol, position))*).toTry.get

  def append(moves: Move*): Either[Throwable, Game] = validateAppend(this, moves*).fold(
    err => Left(new RuntimeException(err.mkString(", "))),
    _.foldLeft[Either[Throwable, Game]](Right(this)) {
      case (Right(game), move @ Move(symbol, position @ (x, y), _)) =>
        Either.cond(
          !game.moves.exists { case Move(_, pos, _) => pos == position },
          game.copy(moves = game.moves :+ move),
          new RuntimeException(s"Can't add move $symbol to $x,$y because if would override existing one.")
        )
      case (left, _)                                                => left
    }
  )

  @targetName("appendFromTuples")
  def append(symbolMoves: (Symbol, Position)*): Either[Throwable, Game] =
    append(symbolMoves.map(Move.of)*)

  @targetName("appendFromEmpty")
  def append(symbolMoves: Seq[Move] = Seq.empty): Either[Throwable, Game] =
    append(symbolMoves*)

  def appendZIO(moves: Move*): Task[Game] = ZIO.fromEither(append(moves*))

object Game:
  def make(
    gid: GID,
    playing: Symbol = defaultPlaying,
    size: Size = Size.default,
    moves: Array[Move] = Array.empty
  ): Game =
    apply(gid, playing, moves, size)

  val empty: Game = make(UUID.randomUUID())
