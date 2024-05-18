package boards.algebra

import boards.graphics.Colour
import boards.algebra.Action.*
import boards.algebra.GameState.{*, given}
import util.math.{Metric, Vec}
import util.math.kernel.{Kernel, Ray}
import util.math.Pos.{*, given}
import util.math.kernel.Kernel.{*, given}
import util.extensions.FunctionOps.{*, given}
import BoardState.*
import boards.algebra.Piece.{*, given}

import scala.collection.immutable.BitSet
import scala.reflect.ClassTag

case class BoardState private (
  pieces: PieceSet,
  numPlayers: Int,
  activePlayer: Int = 0
)(using board: Kernel[?]):
  
  export pieces.{contains, get, belongsTo, isFriendly, isEnemy, isType}
  export board.contains as inBounds
  
  def endTurn(skip: Int = 1): BoardState =
    copy(activePlayer = (activePlayer + skip) % numPlayers)
  def endTurn: BoardState = endTurn()
  
  def inactivePlayers: Seq[Int] = Seq.range(0, numPlayers).filter(_ != activePlayer)
  def nextPlayer: Int = (activePlayer + 1) % numPlayers
  
  def place
    (owner: Int)
    (placements: (Kernel[?], PieceType | Iterable[PieceType])*)
  : BoardState =
    copy(pieces = pieces.place(owner)(placements*))
  
  def place(placements: (Kernel[?], PieceType | Iterable[PieceType])*): BoardState =
    place(activePlayer)(placements*)
  
  def move(moves: (Kernel[?], Kernel[?])*): BoardState =
    copy(pieces = pieces.move(moves*))
  
  def destroy(positions: Kernel[?]*): BoardState =
    copy(pieces = pieces.destroy(positions*))
  
  def draw: String =
    board.offset.y.until(board.extent.y).map: y =>
      board.offset.x.until(board.extent.x).map: x =>
        (board.label(x, y), pieces.get(Vec(x, y))) match
          case (Some(_), Some(piece)) => piece.state.getClass.getSimpleName.apply(0)
          case (Some(_), None) => "."
          case (None, _) => " "
      .mkString
    .reverse.mkString("\n")
    
object BoardState:
  
  def apply(numPlayers: Int)(using board: Kernel[Colour]): BoardState =
    BoardState(PieceSet.empty(using board), numPlayers)
  
  given Conversion[GameState, BoardState] with
    def apply(state: GameState): BoardState = state.boardState
  
  given (using state: GameState): Conversion[BoardState, GameState] with
    def apply(board: BoardState): GameState = state.withBoard(board)
  
  given (using state: GameState): BoardState = state.boardState

  