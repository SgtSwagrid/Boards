package boards.algebra

import boards.algebra.rules.Rule
import boards.algebra.rules.Rule
import boards.imports.games.{*, given}
import boards.imports.math.{*, given}
import boards.algebra.state.{InstantaneousState, Piece}
import boards.math.Ray

import scala.annotation.{tailrec, targetName}

sealed trait Action:
  
  def enact(state: InstantaneousState): InstantaneousState
  
  @targetName("causes")
  def ~> (state: InstantaneousState): Rule = ???//toRule |> R.setState(state)
  
  def toRule: Rule
  
  def hash: String
  
  def isSkip: Boolean = false

object Action:
  
  def skip: Skip.type = Skip
  
  case class Place (
    owner: Game.PlayerId,
    piece: Piece.PieceType,
    position: VecI,
  ) extends Action:
    
    def enact(state: InstantaneousState) =
      state.updatePieces(_.withPiece(Piece(piece, position, owner)))
      
    override def toString = s"$piece |-> $position"
    
    def toRule: Rule = Generator.place(owner)(piece -> position)
    
    def hash: String = s"P ${piece.hash} $position"
  
  case class Move (
    piece: Piece,
    from: VecI,
    to: VecI,
  ) extends Action:
    
    def enact(state: InstantaneousState) =
      state.updatePieces(_.withMove(from, to))
    
    def step: VecI = to - from
    def direction: VecI = from.directionTo(to)
    def path: Ray = Ray.between(from, to)
    def midpoint: VecI = from.midpoint(to)
    
    override def toString = s"$from |-> $to"
    
    def toRule: Rule = Generator.move(from -> to)
    
    def hash: String = s"M $from $to"
  
  case class Destroy (
    piece: Piece
  ) extends Action:
    
    def enact(state: InstantaneousState) =
      given InstantaneousState = state
      state.updatePieces(_.withoutPiece(piece.position))
    
    def toRule: Rule = Generator.destroy(piece.position)
    
    def hash: String = s"D ${piece.position}"
    
  case object Skip extends Action:
    def enact(state: InstantaneousState) = state
    def toRule: Rule = Rule.skip
    def hash: String = "S"
    override def isSkip: true = true