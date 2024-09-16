package boards.algebra

import boards.imports.games.{*, given}
import boards.imports.math.{*, given}

import boards.algebra.Shortcuts.given_Conversion_PieceSet_InstantaneousState
import boards.algebra.Shortcuts.given_Conversion_Action_Rule
import boards.algebra.Shortcuts.given_Kernel_

import scala.annotation.{tailrec, targetName}

sealed trait Action:
  
  def enact(state: InstantaneousState): InstantaneousState
  
  @targetName("causes")
  def ~> (state: InstantaneousState): Rule = this.after(_ => state)

object Action:
  
  case class Place (
    owner: Int,
    piece: boards.algebra.Piece.PieceType,
    position: VecI
  ) extends Action:
    
    def enact(state: InstantaneousState) =
      given InstantaneousState = state
      state.pieces.insert(owner)(piece -> position)
      
    override def toString = s"$piece |-> $position"
  
  case class Move (
    piece: Piece,
    from: VecI,
    to: VecI
  ) extends Action:
    
    def enact(state: InstantaneousState) =
      given InstantaneousState = state
      state.pieces.relocate(from -> to)
    
    def step: VecI = to - from
    def direction: VecI = from.directionTo(to)
    def path: Ray = Ray.between(from, to)
    def midpoint: VecI = from.midpoint(to)
    
    override def toString = s"$from |-> $to"
  
  case class Destroy (
    piece: Piece
  ) extends Action:
    
    def enact(state: InstantaneousState) =
      given InstantaneousState = state
      state.pieces.remove(piece.position)
    
  case object NoOp extends Action:
    def enact(state: InstantaneousState) = state
    
