package boards.algebra

import boards.graphics.Colour
import boards.algebra.Action.*
import boards.algebra.GameState.{*, given}
import util.math.{Metric, Vec}
import util.math.kernel.{Kernel, Ray}
import util.math.kernel.Kernel.{*, given}
import util.extensions.FunctionOps.{*, given}
import InstantaneousState.*
import boards.algebra.Piece.{*, given}

import scala.collection.immutable.BitSet
import scala.reflect.ClassTag

case class InstantaneousState (
  board: Kernel[Colour],
  pieces: PieceSet,
  numPlayers: Int,
  activePlayer: Int = 0,
):
  
  export board.contains as inBounds
  
  def endTurn(skip: Int = 1): InstantaneousState =
    copy(activePlayer = (activePlayer + skip) % numPlayers)
  def endTurn: InstantaneousState = endTurn()
  
  def inactivePlayers: Seq[Int] = Seq.range(0, numPlayers).filter(_ != activePlayer)
  def nextPlayer: Int = (activePlayer + 1) % numPlayers
  
  def withPieces(pieces: PieceSet): InstantaneousState = copy(pieces = pieces)
  
  def draw: String =
    board.offset.y.until(board.extent.y).map: y =>
      board.offset.x.until(board.extent.x).map: x =>
        (board.label(x, y), pieces.get(Vec(x, y))) match
          case (Some(_), Some(piece)) => piece.pieceType.getClass.getSimpleName.apply(0)
          case (Some(_), None) => "."
          case (None, _) => " "
      .mkString
    .reverse.mkString("\n")
    
object InstantaneousState:
  
  def initial(board: Kernel[Colour], numPlayers: Int): InstantaneousState =
    InstantaneousState(board, PieceSet.empty(using board), numPlayers)
    
  def empty: InstantaneousState =
    new InstantaneousState(Kernel.empty, PieceSet.empty(using Kernel.empty), 0)
  
  given Conversion[GameState, InstantaneousState] with
    def apply(state: GameState): InstantaneousState = state.now
  
  given (using state: GameState): Conversion[InstantaneousState, GameState] with
    def apply(board: InstantaneousState): GameState = state.withBoard(board)
  
  given (using state: GameState): InstantaneousState = state.now

  given (using state: InstantaneousState): Kernel[?] = state.board
  given (using state: InstantaneousState): PieceSet = state.pieces