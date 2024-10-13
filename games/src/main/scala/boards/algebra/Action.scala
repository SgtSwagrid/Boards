package boards.algebra

import boards.imports.games.{*, given}
import boards.imports.math.{*, given}

import boards.algebra.shortcuts.given_Conversion_PieceSet_InstantaneousState
import boards.algebra.shortcuts.given_Conversion_Action_Rule
import boards.algebra.shortcuts.given_Kernel_

import scala.annotation.{tailrec, targetName}

sealed trait Action:
  
  def enact(state: InstantaneousState): InstantaneousState
  
  @targetName("causes")
  def ~> (state: InstantaneousState): Rule = this.after(_ => state)
  
  def hash: String

object Action:
  
  case class Place (
    owner: Game.PlayerId,
    piece: boards.algebra.Piece.PieceType,
    position: VecI
  ) extends Action:
    
    def enact(state: InstantaneousState) =
      given InstantaneousState = state
      state.pieces.insert(owner)(piece -> position)
      
    override def toString = s"$piece |-> $position"
    
    def hash: String = s"P ${piece.hash} $position"
  
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
    
    def hash: String = s"M $from $to"
  
  case class Destroy (
    piece: Piece
  ) extends Action:
    
    def enact(state: InstantaneousState) =
      given InstantaneousState = state
      state.pieces.remove(piece.position)
      
    def hash: String = s"D ${piece.position}"
    
  case object NoOp extends Action:
    def enact(state: InstantaneousState) = state
    def hash: String = "N"