package boards.dsl.states

import boards.dsl.meta.Game.GameConfig
import boards.dsl.meta.PlayerId.PlayerId
import boards.graphics.Colour
import boards.imports.games.{*, given}
import boards.imports.math.{*, given}
import boards.math.region.{Region, RegionMap}
import boards.math.region.RegionMap.RegionMapI
import boards.dsl.meta.Game.Board
import boards.dsl.meta.PlayerId.{+, -}

import scala.collection.immutable.BitSet
import scala.reflect.ClassTag

case class InstantaneousState (
  board: RegionMapI[Colour],
  pieces: PieceState,
  config: GameConfig,
  activePlayer: PlayerId = PlayerId.initial,
):
  
  given InstantaneousState = this
  given GameConfig = config
  export board.contains as inBounds
  
  def endTurn(skip: Int = 1): InstantaneousState =
    copy(activePlayer = activePlayer + skip)
  def endTurn: InstantaneousState = endTurn()
  
  def players: Seq[PlayerId] =
    Seq.range(0, config.numPlayers)
      .map(PlayerId.apply)
  def otherPlayers(playerIds: PlayerId*): Seq[PlayerId] =
    players.filter(p => !playerIds.contains(p))
  def inactivePlayers: Seq[PlayerId] =
    otherPlayers(activePlayer)
  def nextPlayer: PlayerId = activePlayer.next
  def previousPlayer: PlayerId = activePlayer.previous
  
  def withPieces(pieces: PieceState): InstantaneousState = copy(pieces = pieces)
  def updatePieces(f: PieceState => PieceState): InstantaneousState = copy(pieces = f(pieces))
  
  override def toString = pieces.toString
    
object InstantaneousState:
  
  def initial(board: Board, config: GameConfig): InstantaneousState =
    InstantaneousState(board, PieceState.forBoard(board), config)
    
  def empty: InstantaneousState =
    new InstantaneousState(RegionMap.empty, PieceState.empty, GameConfig(0))