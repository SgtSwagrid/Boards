package boards.dsl.pieces

import boards.dsl.meta.PlayerId.PlayerId
import boards.dsl.states.InstantaneousState

trait OfPlayer[+X]:
  
  def ofPlayer(playerIds: PlayerId*): X
  
  def ofOtherPlayers(playerIds: PlayerId*)(using state: InstantaneousState): X =
    ofPlayer(state.otherPlayers(playerIds*)*)
  
  def ofActivePlayer(using state: InstantaneousState): X =
    ofPlayer(state.activePlayer)
  
  def ofInactivePlayers(using state: InstantaneousState): X =
    ofPlayer(state.inactivePlayers *)
  
  def ofNextPlayer(using state: InstantaneousState): X =
    ofPlayer(state.nextPlayer)
  
  def ofPreviousPlayer(using state: InstantaneousState): X =
    ofPlayer(state.previousPlayer)