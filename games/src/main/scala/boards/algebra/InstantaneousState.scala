package boards.algebra

import boards.imports.games.{*, given}
import boards.imports.math.{*, given}
import boards.algebra.Game.{*, given}

import scala.collection.immutable.BitSet
import scala.reflect.ClassTag

case class InstantaneousState (
  board: Kernel[boards.graphics.Colour],
  pieces: PieceSet,
  config: GameConfig,
  activePlayer: Game.PlayerId = PlayerId(0),
):
  
  given InstantaneousState = this
  export board.contains as inBounds
  
  def endTurn(skip: Int = 1): InstantaneousState =
    copy(activePlayer = activePlayer + skip)
  def endTurn: InstantaneousState = endTurn()
  
  def inactivePlayers: Seq[PlayerId] = Seq.range(0, config.numPlayers)
    .map(PlayerId.apply)
    .filter(_ != activePlayer)
  def nextPlayer: PlayerId = activePlayer + 1
  
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
  
  def initial(board: Kernel[boards.graphics.Colour], config: GameConfig): InstantaneousState =
    InstantaneousState(board, PieceSet.empty(using board), config)
    
  def empty: InstantaneousState =
    new InstantaneousState(Kernel.empty, PieceSet.empty(using Kernel.empty), GameConfig(0))
    
  given (using state: InstantaneousState): GameConfig = state.config