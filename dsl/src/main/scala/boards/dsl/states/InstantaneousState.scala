package boards.dsl.states

import boards.dsl.meta.Game.{Board, GameConfig}
import boards.dsl.meta.PlayerRef.{+, PlayerId, PlayerRef}
import boards.dsl.pieces.PieceState
import boards.math.vector.Embedding
import boards.math.vector.Vec.{HasVecI, VecI}

/**
  * The state of a game at a specific moment in time, without any history.
  *
  * @param board The game board, describing legal piece positions and layout properties.
  * @param pieces The current set of all pieces on the board.
  * @param config The configuration of the game, including the number of players and any custom settings.
  *               Remains unchanged throughout the course of the game.
  * @param activePlayer The ID of the player whose turn it currently is.
  *
  * @author Alec Dorrington
  */
case class InstantaneousState (
  board: Embedding = Embedding.empty,
  pieces: PieceState = PieceState.empty,
  config: GameConfig = GameConfig(0, Map.empty),
  activePlayer: PlayerId = PlayerId.initial,
):
  
  given InstantaneousState = this
  given GameConfig = config
  
  def inBounds (v: VecI): Boolean = board.containsLogical(v)
  
  def endTurn (skip: Int = 1): InstantaneousState =
    copy(activePlayer = activePlayer + skip)
    
  def endTurn: InstantaneousState = endTurn()
  
  def players: Seq[PlayerId] =
    Seq.range(0, config.numPlayers).map(PlayerId.apply)
    
  def otherPlayers (players: PlayerRef*): Seq[PlayerRef] =
    players.filter(p => !players.contains(p))
    
  def inactivePlayers: Seq[PlayerRef] =
    otherPlayers(activePlayer)
    
  def nextPlayer: PlayerId = activePlayer.next
  
  def previousPlayer: PlayerId = activePlayer.previous
  
  private[dsl] def withPieces (pieces: PieceState): InstantaneousState = copy(pieces = pieces)
  private[dsl] def updatePieces (f: PieceState => PieceState): InstantaneousState = copy(pieces = f(pieces))
  
  private[dsl] def withBoard (board: Board): InstantaneousState =
    copy(board = board, pieces = PieceState.forBoard(board))
  
  override def toString = pieces.toString
    
object InstantaneousState:
  
  def initial (board: Board) (using config: GameConfig): InstantaneousState =
    InstantaneousState(board, PieceState.forBoard(board), config)
    
  def empty: InstantaneousState =
    new InstantaneousState()