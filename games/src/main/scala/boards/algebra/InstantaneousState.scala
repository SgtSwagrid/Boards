package boards.algebra

import boards.imports.games.{*, given}
import boards.imports.math.{*, given}

import scala.collection.immutable.BitSet
import scala.reflect.ClassTag

case class InstantaneousState (
  board: Kernel[boards.graphics.Colour],
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
    
  def diff(that: InstantaneousState): Iterator[VecI] =
    board.positions.filter(v => pieces.piecesByPos.get(v) != that.pieces.piecesByPos.get(v))
    
object InstantaneousState:
  
  def initial(board: Kernel[Colour], numPlayers: Int): InstantaneousState =
    InstantaneousState(board, PieceSet.empty(using board), numPlayers)
    
  def empty: InstantaneousState =
    new InstantaneousState(Kernel.empty, PieceSet.empty(using Kernel.empty), 0)