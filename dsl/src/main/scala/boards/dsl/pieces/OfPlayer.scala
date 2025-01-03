package boards.dsl.pieces

import boards.dsl.meta.PlayerRef.PlayerRef
import boards.dsl.states.InstantaneousState

trait OfPlayer[+X]:
  
  def ofPlayer(player: PlayerRef*): X
  
  def ofOtherPlayers(players: PlayerRef*)(using state: InstantaneousState): X =
    ofPlayer(state.otherPlayers(players*)*)
  
  def ofActivePlayer(using state: InstantaneousState): X =
    ofPlayer(state.activePlayer)
  
  def ofInactivePlayers(using state: InstantaneousState): X =
    ofPlayer(state.inactivePlayers *)
  
  def ofNextPlayer(using state: InstantaneousState): X =
    ofPlayer(state.nextPlayer)
  
  def ofPreviousPlayer(using state: InstantaneousState): X =
    ofPlayer(state.previousPlayer)