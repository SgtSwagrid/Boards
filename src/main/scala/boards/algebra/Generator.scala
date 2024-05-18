package boards.algebra

import boards.GameImports.{*, given}
import boards.algebra.Piece.PieceType
import util.math.kernel.{Kernel, Ray}

sealed trait Generator extends Rule:
  import Generator.*
  
  def next(state: NonFinalState) =
    actions(state).map: action =>
      InterimState(action.enact(state), action, state, Rule.skip)
  
  def updateGenerators(filter: PartialFunction[Generator, Rule]) =
    filter.lift(this).getOrElse(Generator.none)

object Generator:
  
  def none: Generator = EmptyGenerator
  def skip: Generator = SkipGenerator
  
  def place
    (owner: Int)
    (placements: (Kernel[?], PieceType | Iterable[PieceType])*)
  : Rule =
    
    placements.map: (kernel, pieces) =>
      val pieceSet = pieces match
        case p: PieceType => Set(p)
        case p: Iterable[?] => p.toSet.asInstanceOf[Set[PieceType]]
      PlaceGenerator(owner, pieceSet, kernel)
  
  def place(placements: (Kernel[?], PieceType | Iterable[PieceType])*): Rule =
    Rule.when(state => place(state.activePlayer)(placements*))
  
  def move(moves: (Kernel[?], Kernel[?])*): Rule =
    moves.map(MoveGenerator.apply)
  
  def destroy(positions: Kernel[?]*): Rule =
    positions.map(DestroyGenerator.apply)
    
  case object EmptyGenerator extends Generator:
    def actions(state: NonFinalState) = Iterator.empty
    
  case object SkipGenerator extends Generator:
    def actions(state: NonFinalState) = Iterator(NoOp)
    override def updateGenerators(filter: PartialFunction[Generator, Rule]) = this
  
  case class PlaceGenerator (
    owner: Int,
    pieces: Set[PieceType],
    kernel: Kernel[?]
  ) extends Generator:
    
    def actions(state: NonFinalState) =
      for
        piece <- pieces.iterator
        pos <- kernel.positions if state.inBounds(pos)
      yield Place(owner, piece, pos)
      
  case class MoveGenerator (
    from: Kernel[?],
    to: Kernel[?]
  ) extends Generator:
    
    def actions(state: NonFinalState) =
      for
        from <- from.positions if state.inBounds(from)
        to <- to.positions if state.inBounds(to)
        piece <- state.pieces.get(from)
      yield Move(piece.copy(position=to), from, to)
      
  case class DestroyGenerator (
    kernel: Kernel[?]
  ) extends Generator:
    
    def actions(state: NonFinalState) =
      for
        pos <- kernel.positions if state.inBounds(pos)
        piece <- state.pieces.get(pos)
      yield Destroy(piece)
  
  given Conversion[Action, Rule] with
    def apply(action: Action): Rule =
      action match
        case Place(owner, piece, pos) => place(owner)(pos -> piece)
        case Move(_, from, to) => move(from -> to)
        case Destroy(piece) => destroy(piece)
        case NoOp => skip