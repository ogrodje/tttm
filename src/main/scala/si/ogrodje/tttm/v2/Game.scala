package si.ogrodje.tttm.v2

import eu.timepit.refined.auto.*
import zio.json.*
import zio.prelude.{NonEmptySet, Validation}
import zio.{Duration, RIO, Task, ZIO}

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

@jsonHintNames(SnakeCase)
final case class Move(
  symbol: Symbol,
  position: Position,
  @jsonField("duration_ms") duration: Duration = Duration.Zero,
  @jsonField("player_server_id") playerServerID: Option[PlayerServerID] = None
)
object Move:
  given durationJsonEncoder: JsonEncoder[Duration] = JsonEncoder[Double].contramap(_.toMillis.toDouble)
  given gameJsonEncoder: JsonEncoder[Move]         = DeriveJsonEncoder.gen[Move]

  def of(symbol: Symbol, position: Position): Move = Move(symbol, position)
  def of(tpl: (Symbol, Position)): Move            = Move(tpl._1, tpl._2)

type Moves = Array[Move]

@jsonDiscriminator("type")
enum Status:
  case Tie
  case Pending
  case Won(symbol: Symbol)
  case CrashedBy(symbol: Symbol, message: String)

  override def toString: String = this match
    case Status.Tie                        => "Tie"
    case Status.Pending                    => "Pending"
    case Status.Won(symbol)                => s"Won by $symbol"
    case Status.CrashedBy(symbol, message) => s"Crashed by symbol $symbol with: $message"

object Status:
  given eventJsonEncoder: JsonEncoder[Status] = DeriveJsonEncoder.gen[Status]

enum GameError(val message: String) extends RuntimeException(message):
  case NoMovesPresent                 extends GameError("At least one move is needed.")
  case InvalidSymbol                  extends GameError("Only X or O symbols are allowed")
  case InvalidSequence(symbols: Seq[Symbol])
      extends GameError(s"Invalid sequence of symbols: ${symbols.mkString(", ")}")
  case InvalidFirstMove               extends GameError("First move needs to be X")
  case InvalidAppending(errors: List[GameError])
      extends GameError(s"Invalid appending with errors: ${errors.map(_.message).mkString(", ")} ")
  case CantOverrideMove(symbol: Symbol, position: Position)
      extends GameError(s"Can't add move $symbol to $position because if would override existing one.")
  case AppendError(thMessage: String) extends GameError(s"Invalid appending with error ${thMessage}")

@jsonHintNames(SnakeCase)
final case class Game private (
  gid: GID,
  @jsonField("player_server_x_id") playerServerIDX: PlayerServerID,
  @jsonField("player_server_o_id") playerServerIDO: PlayerServerID,
  playing: Symbol = defaultPlaying,
  moves: Moves = Array.empty[Move],
  size: Size = Size.default,
  crashedBy: Option[(Symbol, String)] = None
):
  import Status.*
  import GameError.*

  private def symbolAt: Position => Option[Symbol] = (x, y) =>
    moves.find { case Move(_, (px, py), _, _) => px == x && py == y }.map(_._1)

  private def arrayOfSymbols(symbol: Symbol): Array[Option[Symbol]] =
    val minRequiredSize: Int = size.value match
      case 3 => 3
      case _ => 4

    (0 until minRequiredSize).toArray.map(_ => Some(symbol))

  def grid: Array[(Option[Symbol], Position)] = for
    x <- (0 until size).toArray
    y <- 0 until size
  yield symbolAt(x, y) -> (x, y)

  def listMoves: Array[Move] = moves

  private def isSubsequence(
    main: Array[Option[Symbol]],
    sub: Array[Option[Symbol]]
  ): Boolean =
    @scala.annotation.tailrec
    def loop(mainIndex: Int, subIndex: Int): Boolean =
      if subIndex >= sub.length then true
      else if mainIndex >= main.length then false
      else
        (main(mainIndex), sub(subIndex)) match
          case (Some(m), Some(s)) if m == s => loop(mainIndex + 1, subIndex + 1)
          case (None, None)                 => loop(mainIndex + 1, subIndex + 1)
          case _                            => loop(mainIndex + 1, subIndex)

    loop(0, 0)

  private def sequenceOf(
    sub: Array[Option[Symbol]]
  )(main: Array[Option[Symbol]]): Boolean = isSubsequence(main, sub)

  private def wonRows(symbol: Symbol): Boolean =
    grid
      .grouped(size)
      .map(_.map(_._1))
      .exists(sequenceOf(arrayOfSymbols(symbol)))

  private def wonColumns(symbol: Symbol): Boolean =
    (0 until size)
      .map(r => (0 until size).map(symbolAt(_, r)))
      .map(_.toArray)
      .exists(sequenceOf(arrayOfSymbols(symbol)))

  private def hasWon(symbol: Symbol): Boolean =
    Seq(wonRows, wonColumns, wonDiagonals).exists(f => f(symbol))

  private def isFull: Boolean = moves.length == size * size

  def emptyPositions: Array[Position] =
    grid.collect { case (None, position) => position }

  def status: Status =
    (crashedBy, hasWon(X) -> hasWon(O)) match
      case (Some(symbol -> message), _) => CrashedBy(symbol, message)
      case (None, true -> false)        => Won(X)
      case (None, false -> true)        => Won(O)
      case (None, true -> true)         => Won(X)
      case _ if isFull                  => Tie
      case _                            => Pending

  private def wonDiagonals(symbol: Symbol): Boolean =
    val minimumSize: Int = size.value match
      case 3 => 3
      case _ => 4

    def diagonallyTopLeftToBottomRight(n: Int, minSize: Int): Array[Array[Position]] =
      generateDiagonal(n, minSize, (i, d) => (i, d - i), (x, y, maxN) => x < maxN && y < maxN)

    def diagonallyTopRightToBottomLeft(n: Int, minSize: Int): Array[Array[Position]] =
      generateDiagonal(n, minSize, (i, d) => (i, (n - 1) - (d - i)), (x, y, _) => x < n && y >= 0)

    def generateDiagonal(
      n: Int,
      minSize: Int,
      coordinates: (Int, Int) => (Int, Int),
      condition: (Int, Int, Int) => Boolean
    ): Array[Array[Position]] =
      val result = scala.collection.mutable.ArrayBuffer[Array[Position]]()
      for d <- 0 until 2 * n - 1 do
        val diagonal = scala.collection.mutable.ArrayBuffer[Position]()
        for i <- 0 to d do
          val (x, y) = coordinates(i, d)
          if condition(x, y, n) then diagonal.append((x, y))
        result.append(diagonal.toArray)
      result.toArray.filter(_.length >= minSize)

    val left  =
      diagonallyTopLeftToBottomRight(size, minimumSize)
        .map(_.map(symbolAt))
        .exists(sequenceOf(arrayOfSymbols(symbol)))
    val right =
      diagonallyTopRightToBottomLeft(size, minimumSize)
        .map(_.map(symbolAt))
        .exists(sequenceOf(arrayOfSymbols(symbol)))

    left || right

  def wonBy(symbol: Symbol): Boolean = status == Status.Won(symbol)
  def isTie: Boolean                 = status == Status.Tie

  def hasCrashedWith(symbol: Symbol): Boolean = status match
    case Status.CrashedBy(`symbol`, _) => true
    case _                             => false

  def hasCrashed: Boolean = status match
    case _: Status.CrashedBy => true
    case _                   => false

  def show: String =
    grid
      .grouped(size)
      .map(_.map { case (Some(symbol), _) => symbol; case _ => " " }.mkString(","))
      .mkString("\n")

  def copyBySwitchingPlaying: Game         = this.copy(playing = oppositeSymbol(playing))
  def copyAsCrashed(message: String): Game = this.copy(crashedBy = Some(playing -> message))

  private def nonEmptyMoves(moves: Seq[Move]): Validation[GameError, Seq[Move]] =
    Validation.fromPredicateWith(NoMovesPresent)(moves)(_.nonEmpty)

  private def validateSymbols(moves: Seq[Move]): Validation[GameError, Seq[Move]] =
    Validation.fromPredicateWith(InvalidSymbol)(moves)(
      _.forall { case Move(symbol, _, _, _) => validSymbols.contains(symbol) }
    )

  private def validSequence(moves: Seq[Move]): Validation[GameError, Seq[Move]] =
    if moves.isEmpty then Validation.succeed(moves)
    else
      Validation
        .fromPredicate(moves)(_.sliding(2).forall {
          case Seq(Move(`X`, _, _, _), Move(`O`, _, _, _)) => true
          case Seq(Move(`O`, _, _, _), Move(`X`, _, _, _)) => true
          case Seq(_, _)                                   => false
          case _                                           => true
        })
        .mapError(_ => InvalidSequence(moves.map(_.symbol)))

  private def validateFirstMove(game: Game, moves: Seq[Move]): Validation[GameError, Seq[Move]] =
    Validation.fromPredicateWith(InvalidFirstMove)((game.moves ++ moves).toSeq)(
      _.headOption.exists { case Move(symbol, _, _, _) => symbol == X }
    )

  private def validateAppend(game: Game, moves: Move*): Validation[GameError, Seq[Move]] =
    nonEmptyMoves(moves.toSeq)
      .flatMap(validateSymbols)
      .flatMap(validSequence)
      .flatMap(validateFirstMove(game, moves).as)

  def appendUnsafe(moves: Move*): Game = append(moves*).toTry.get

  @targetName("appendUnsafeFromTuples")
  def appendUnsafe(moves: (Symbol, Position)*): Game =
    append(moves.map((symbol, position) => Move.of(symbol, position))*).toTry.get

  def append(moves: Move*): Either[GameError, Game] = validateAppend(this, moves*).fold(
    err => Left(InvalidAppending(err.toList)),
    _.foldLeft[Either[GameError, Game]](Right(this)) {
      case (Right(game), move @ Move(symbol, position @ (x, y), _, _)) =>
        Either.cond(
          !game.moves.exists { case Move(_, pos, _, _) => pos == position },
          game.copy(moves = game.moves :+ move),
          CantOverrideMove(symbol, position)
        )
      case (left, _)                                                   =>
        left.left.map(th => AppendError(th.getMessage))
    }
  )

  @targetName("appendFromTuples")
  def append(symbolMoves: (Symbol, Position)*): Either[GameError, Game] =
    append(symbolMoves.map(Move.of)*)

  @targetName("appendFromEmpty")
  def append(symbolMoves: Seq[Move] = Seq.empty): Either[GameError, Game] =
    append(symbolMoves*)

  def appendZIO(moves: Move*): ZIO[Any, GameError, Game] = ZIO.fromEither(append(moves*))

object Game:
  given sizeEncoder: JsonEncoder[Size]     = JsonEncoder[Int].contramap(_.value)
  given gameJsonEncoder: JsonEncoder[Game] = DeriveJsonEncoder.gen[Game]

  def make(
    gid: GID,
    playerServerIDX: PlayerServerID,
    playerServerIDO: PlayerServerID,
    playing: Symbol = defaultPlaying,
    size: Size = Size.default,
    moves: Array[Move] = Array.empty
  ): Game =
    apply(gid, playerServerIDX, playerServerIDO, playing, moves, size)

  val empty: Game              = make(
    UUID.randomUUID(),
    playerServerIDX = "pid-" + X,
    playerServerIDO = "pid-" + O
  )
  def ofSize(size: Size): Game = empty.copy(size = size)
