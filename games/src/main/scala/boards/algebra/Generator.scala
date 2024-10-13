package boards.algebra

import boards.imports.games.{*, given}
import boards.imports.math.{*, given}

sealed trait Generator extends Rule:
  import Generator.*
  
  def next(state: NonFinalState) =
    actions(state).map: action =>
      InterimState(action.enact(state.now), action, state, Rule.skip)
  
  def updateGenerators(filter: PartialFunction[Generator, Rule]) =
    filter.lift(this).getOrElse(Generator.none)

object Generator:
  
  import boards.algebra.shortcuts.given_Conversion_Iterable_Rule
  
  def none: Generator = EmptyGenerator
  def skip: Generator = SkipGenerator
  
  def place
    (owner: PlayerId)
    (placements: (Piece.PieceType | Iterable[Piece.PieceType], Kernel[?])*)
  : Rule =
    
    placements.map: (pieces, kernel) =>
      val pieceSet = pieces match
        case p: Piece.PieceType => Set(p)
        case p: Iterable[?] => p.toSet.asInstanceOf[Set[Piece.PieceType]]
      PlaceGenerator(owner, pieceSet, kernel)
  
  def place(placements: (Piece.PieceType | Iterable[Piece.PieceType], Kernel[?])*): Rule =
    Rule.when(state => place(state.now.activePlayer)(placements*))
  
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
    owner: PlayerId,
    pieces: Set[Piece.PieceType],
    kernel: Kernel[?]
  ) extends Generator:
    
    def actions(state: NonFinalState) =
      for
        piece <- pieces.iterator
        pos <- kernel.positions if state.now.inBounds(pos)
      yield Place(owner, piece, pos)
      
  case class MoveGenerator (
    from: Kernel[?],
    to: Kernel[?]
  ) extends Generator:
    
    def actions(state: NonFinalState) =
      for
        from <- from.positions if state.now.inBounds(from)
        to <- to.positions if state.now.inBounds(to)
        piece <- state.now.pieces.get(from)
      yield Move(piece.copy(position=to), from, to)
      
  case class DestroyGenerator (
    kernel: Kernel[?]
  ) extends Generator:
    
    def actions(state: NonFinalState) =
      for
        pos <- kernel.positions if state.now.inBounds(pos)
        piece <- state.now.pieces.get(pos)
      yield Destroy(piece)