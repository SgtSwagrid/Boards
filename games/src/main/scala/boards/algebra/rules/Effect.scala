package boards.algebra.rules

import boards.imports.games.{*, given}
import boards.imports.math.{*, given}
import boards.algebra.rules.Rule.Query

/**
 * A rule representing a modification to the game state
 * that wasn't directly caused by player input.
 *
 * A transformer works similarly to a generator,
 * except in response to fictitious 'skip' actions instead of actual player-caused actions.
 */
trait Effect extends Rule:
  def toString(state: GameState, depth: Int): String = "."

object Effect:
  
  def endTurn: Rule =
    MapEffect(_.now.endTurn)
  
  def stop(outcome: GameState ?=> Outcome): Rule =
    Rule.switch(TerminalEffect(outcome))
    
  def insert
    (owner: PlayerId)
    (insertions: (PieceType | Iterable[PieceType], Ker)*)
  : Rule = Rule.sequence (
    insertions.map:
      case (piece: PieceType, region) => InsertEffect(owner, IndexedSeq(piece), region)
      case (pieces: Iterable[PieceType], region) => InsertEffect(owner, pieces.toIndexedSeq, region)
  *)
  
  def insert
    (insertions: (PieceType | Iterable[PieceType], Ker)*)
    (using owner: PlayerId)
  : Rule = insert(owner)(insertions*)
  
  def relocate(relocations: (Ker, Ker)*): Rule =
    Rule.sequence(relocations.map(RelocateEffect.apply)*)
    
  def remove(regions: Ker*): Rule =
    Rule.sequence(regions.map(RemoveEffect.apply)*)
  
  /**
   * A rule which updates the instantaneous state in some arbitrary way.
   */
  private[algebra] sealed trait MapEffect extends Effect:
    
    def next(state: GameState, query: Query): Iterator[GameState] = state match
      case _: FinalState => Iterator.empty
      case state: NonFinalState =>
        Iterator.single(InterimState(transform(state), Action.skip, state, Rule.skip))
      
    protected def transform(state: GameState): InstantaneousState
    
  object MapEffect:
    def apply(f: GameState => InstantaneousState): MapEffect =
      new MapEffect { def transform(state: GameState) = f(state) }
    
  private[algebra] sealed trait PieceEffect extends MapEffect:
    
    protected final def transform(state: GameState): InstantaneousState =
      state.now.withPieces(transform(state.now.pieces))
    
    protected def transform(pieces: PieceSet): PieceSet
    
    // Optimisation: Not required for correctness!
    override def |> (that: => Rule): Rule = that match
      case that: PieceEffect =>
        PieceEffect(state => that.transform(this.transform(state)))
      case _ => super.|>(that)
      
  object PieceEffect:
    def apply(f: PieceSet => PieceSet): PieceEffect =
      new PieceEffect { def transform(pieces: PieceSet) = f(pieces) }
  
  private[algebra] case class InsertEffect (
    owner: PlayerId,
    pieceTypes: IndexedSeq[PieceType],
    region: Ker,
  ) extends PieceEffect:
    
    def transform(pieceSet: PieceSet): PieceSet =
      region.positions.zipWithIndex.map: (pos, i) =>
        Piece(pieceTypes(i % pieceTypes.size), pos, owner)
      .foldLeft(pieceSet)(_.withPiece(_))
      
  private[algebra] case class RelocateEffect (
    from: Ker,
    to: Ker,
  ) extends PieceEffect:
    
    def transform(pieces: PieceSet): PieceSet =
      from.positions.zip(to.positions).map: (from, to) =>
        (pieces.get(from), to)
      .foldLeft(from.positions.foldLeft(pieces)(_.withoutPiece(_))):
        case (pieces, (Some(piece), to)) =>
          pieces.withPiece(piece.copy(position = to, hasMoved = true))
        case (pieces, _) => pieces
        
  private[algebra] case class RemoveEffect (
    region: Ker,
  ) extends PieceEffect:
    
    def transform(pieces: PieceSet): PieceSet =
      region.positions.foldLeft(pieces)(_.withoutPiece(_))
  
  /**
   * A rule which immediately triggers the end of the game.
   *
   * @param outcome the outcome of the game, i.e. who won?
   */
  private[algebra] case class TerminalEffect (
    outcome: Outcome,
  ) extends Effect:
    
    def next(state: GameState, query: Query): Iterator[GameState] = state match
      case _: FinalState => Iterator.empty
      case state: NonFinalState =>
        Iterator.single(FinalState(state.now, Action.skip, state, outcome))