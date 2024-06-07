package boards.algebra

import boards.algebra.Piece.{*, given}
import boards.algebra.Rule.{*, given}
import boards.algebra.Generator.{*, given}
import boards.algebra.BoardState.given
import util.math.kernel.{Kernel, Ray}
import util.math.kernel.Kernel.{*, given}
import util.math.Pos.{*, given}
import util.extensions.Conversions.given

import scala.annotation.{tailrec, targetName}

sealed trait Action:
  
  def enact(state: BoardState): BoardState
  
  @targetName("causes")
  def ~> (state: BoardState): Rule = this.after(_ => state)

object Action:

  case class Place (
    owner: Int,
    piece: PieceType,
    position: Pos
  ) extends Action:
    
    inline def enact(state: BoardState) =
      given BoardState = state
      state.pieces.insert(owner)(piece -> position)
      
    override def toString = s"$piece |-> $position"
  
  case class Move (
    piece: Piece,
    from: Pos,
    to: Pos
  ) extends Action:
    
    inline def enact(state: BoardState) =
      given BoardState = state
      state.pieces.relocate(from -> to)
    
    inline def step: Pos = to - from
    inline def direction: Pos = from.directionTo(to)
    inline def path: Ray = Ray.between(from, to)
    inline def midpoint: Pos = from.midpoint(to)
    
    override def toString = s"$from |-> $to"
  
  case class Destroy (
    piece: Piece
  ) extends Action:
    
    inline def enact(state: BoardState) =
      given BoardState = state
      state.pieces.remove(piece)
    
  case object NoOp extends Action:
    inline def enact(state: BoardState) = state
    
  