package boards.algebra.rules

import boards.imports.games.{*, given}
import boards.imports.math.{*, given}
import boards.algebra.rules.Rule.Query

/**
 * A rule for enumerating all actions of a particular kind.
 * Generators form the leaf nodes of the rule tree.
 */
trait Generator extends Rule:
  
  final def next(state: GameState, query: Query): Iterator[GameState] =
    actions(state, query).map: action =>
      InterimState(action.enact(state.now), action, state.asInstanceOf[NonFinalState], Rule.skip)
      
  override final def actions(state: GameState, query: Query): Iterator[Action] =
    state match
      case _: FinalState => Iterator.empty
      case state: NonFinalState =>
        query.lift(this).map(_.generate(state)).getOrElse(Iterator.empty)
    
  protected def generate(state: GameState): Iterator[Action]

object Generator:
  
  def place
    (owner: PlayerId)
    (placements: (PieceType | Iterable[PieceType], Ker)*)
  : Rule = Rule.union (
    placements.map:
      case (piece: PieceType, region) => PlaceGenerator(owner, Some(piece), region)
      case (pieces: Iterable[PieceType], region) => PlaceGenerator(owner, pieces, region)
  *)
  
  def place
    (placements: (PieceType | Iterable[PieceType], Ker)*)
    (using owner: PlayerId)
  : Rule = place(owner)(placements*)
  
  def move(moves: (Ker, Ker)*): Rule =
    Rule.union(moves.map(MoveGenerator.apply)*)
    
  def destroy(regions: Ker*): Rule =
    Rule.union(regions.map(DestroyGenerator.apply)*)
  
  /**
   * Generates all possible piece placements in a region.
   *
   * @param owner the owner of the pieces.
   * @param pieces the set of available piece types.
   * @param region the board region that may be placed in.
   */
  private[algebra] case class PlaceGenerator (
    owner: PlayerId,
    pieces: Iterable[PieceType],
    region: Ker,
  ) extends Generator:
    
    def generate(state: GameState): Iterator[Action] = for
      pos <- region.positions
      if state.now.inBounds(pos)
      piece <- pieces.iterator
    yield Place(owner, piece, pos)
  
  /**
   * Generates all possible piece movements from one region to another.
   *
   * @param from the board region that may be moved from.
   * @param to the board region that may be moved to.
   */
  private[algebra] case class MoveGenerator (
    from: Ker,
    to: Ker,
  ) extends Generator:
    
    def generate(state: GameState): Iterator[Action] = for
      from <- from.positions
      if state.now.inBounds(from)
      piece <- state.now.pieces.get(from).iterator
      to <- to.positions
      if state.now.inBounds(to)
    yield Move(piece.copy(position = to), from, to)
  
  /**
   * Generates all possible piece removals from a region.
   *
   * @param region the board region that may be removed from.
   */
  private[algebra] case class DestroyGenerator (
    region: Ker,
  ) extends Generator:
    
    def generate(state: GameState): Iterator[Action] = for
      pos <- region.positions
      if state.now.inBounds(pos)
      piece <- state.now.pieces.get(pos).iterator
    yield Destroy(piece)