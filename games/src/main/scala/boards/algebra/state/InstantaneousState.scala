package boards.algebra.state

import boards.algebra.Game.GameConfig
import boards.graphics.Colour
import boards.imports.games.{*, given}
import boards.imports.math.{*, given}
import boards.algebra.Game.PlayerId
import boards.math.Region

import scala.collection.immutable.BitSet
import scala.reflect.ClassTag

case class InstantaneousState (
  board: Region[Int, Colour],
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
  def updatePieces(f: PieceSet => PieceSet): InstantaneousState = copy(pieces = f(pieces))
    
  def diff(that: InstantaneousState): Iterator[VecI] =
    board.positions.filter(v => pieces.piecesByPos.get(v) != that.pieces.piecesByPos.get(v))
    
object InstantaneousState:
  
  def initial(board: Region[Int, Colour], config: GameConfig): InstantaneousState =
    InstantaneousState(board, PieceSet.empty(using board), config)
    
  def empty: InstantaneousState =
    new InstantaneousState(Region.empty, PieceSet.empty(using Region.empty), GameConfig(0))
    
  given (using state: InstantaneousState): GameConfig = state.config