package boards.dsl.states

import boards.dsl.meta.Game.GameConfig
import boards.graphics.Colour
import boards.imports.games.{*, given}
import boards.imports.math.{*, given}
import boards.math.region.{Region, RegionMap}
import boards.math.region.RegionMap.RegionMapI
import boards.dsl.meta.Game.Board
import boards.dsl.meta.PlayerRef.+
import boards.dsl.meta.PlayerRef.{PlayerId, PlayerRef}

import scala.collection.immutable.BitSet
import scala.reflect.ClassTag

case class InstantaneousState (
  board: RegionMapI[Colour] = RegionMap.empty,
  pieces: PieceState = PieceState.empty,
  config: GameConfig = GameConfig(0, Map.empty),
  activePlayer: PlayerId = PlayerId.initial,
):
  
  given InstantaneousState = this
  given GameConfig = config
  export board.contains as inBounds
  
  def endTurn(skip: Int = 1): InstantaneousState =
    copy(activePlayer = activePlayer + skip)
  def endTurn: InstantaneousState = endTurn()
  
  def players: Seq[PlayerId] =
    Seq.range(0, config.numPlayers).map(PlayerId.apply)
  def otherPlayers(players: PlayerRef*): Seq[PlayerRef] =
    players.filter(p => !players.contains(p))
  def inactivePlayers: Seq[PlayerRef] =
    otherPlayers(activePlayer)
  def nextPlayer: PlayerId = activePlayer.next
  def previousPlayer: PlayerId = activePlayer.previous
  
  def withPieces(pieces: PieceState): InstantaneousState = copy(pieces = pieces)
  def updatePieces(f: PieceState => PieceState): InstantaneousState = copy(pieces = f(pieces))
  
  def withBoard(board: Board): InstantaneousState =
    copy(board = board, pieces = PieceState.forBoard(board))
  
  override def toString = pieces.toString
    
object InstantaneousState:
  
  def initial(board: Board, config: GameConfig): InstantaneousState =
    InstantaneousState(board, PieceState.forBoard(board), config)
    
  def empty: InstantaneousState =
    new InstantaneousState()