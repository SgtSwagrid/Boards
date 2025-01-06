package boards.dsl.pieces

import boards.dsl.meta.PlayerRef.PlayerRef
import boards.dsl.states.InstantaneousState

/** This trait helpers for obtaining marginally shorter syntax.
  *
  * If some quantity `X` can be queried for each player,
  * then it is helpful to be able to query it for certain specific players.
  */
trait OfPlayer[+X]:
  
  def ofPlayer(player: PlayerRef*): X
  
  /** Finds those objects belonging to any player besides those specified. */
  def ofOtherPlayers(players: PlayerRef*)(using state: InstantaneousState): X =
    ofPlayer(state.otherPlayers(players*)*)
    
   /** Finds those objects belonging to the player whose turn it currently is. */
  def ofActivePlayer(using state: InstantaneousState): X =
    ofPlayer(state.activePlayer)
  
  /** Finds those objects belonging to all players besides the player whose turn it is. */
  def ofInactivePlayers(using state: InstantaneousState): X =
    ofPlayer(state.inactivePlayers*)
  
  /** Finds those objects belonging to the player immediately following the current player in the turn order. */
  def ofNextPlayer(using state: InstantaneousState): X =
    ofPlayer(state.nextPlayer)
  
  /** Finds those objects belonging to the player immediately preceding the current player in the turn order. */
  def ofPreviousPlayer(using state: InstantaneousState): X =
    ofPlayer(state.previousPlayer)