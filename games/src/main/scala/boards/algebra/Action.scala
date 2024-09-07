package boards.algebra

import boards.algebra.Piece.{*, given}
import boards.algebra.Rule.{*, given}
import boards.algebra.Generator.{*, given}
import boards.algebra.InstantaneousState.given
import util.math.kernel.{Kernel, Ray}
import util.math.kernel.Kernel.{*, given}
import util.extensions.Conversions.given
import util.math.Vec.VecI

import scala.annotation.{tailrec, targetName}

sealed trait Action:
  
  def enact(state: InstantaneousState): InstantaneousState
  
  @targetName("causes")
  def ~> (state: InstantaneousState): Rule = this.after(_ => state)

object Action:
  
  case class Place (
    owner: Int,
    piece: PieceType,
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
      state.pieces.remove(piece)
    
  case object NoOp extends Action:
    def enact(state: InstantaneousState) = state
    
