package boards.dsl.rules

import boards.imports.games.{*, given}
import boards.imports.math.{*, given}
import boards.math.region.Region.HasRegionI
import Effect.*
import boards.math.region.Vec.HasVecI
import boards.dsl.shortcuts.State
import boards.dsl.pieces.{Piece, PieceFilter, PieceType}

import scala.collection.mutable
import boards.dsl.shortcuts.{State, given_HistoryState, piece}

/**
 * A `Rule` for modifying the `InstantaneousState` in some regular manner.
 * All state updates must occur via an `Effect`.
 */
sealed trait Effect extends Rule:
  
  private[dsl] final def successors(state: HistoryState) =
    LazyList.from(effect(state))
    
  private[dsl] override def effect(state: HistoryState): Some[GameState]
  
  final def |> (that: => Effect): Effect =
    if this != Effect.identity
    then SequenceEffect(this, that)
    else that
  
  def structure(state: HistoryState) = "."

object Effect:
  
  val identity: Effect = IdentityEffect
  
  def apply(brancher: HistoryState ?=> Effect): Effect =
    SwitchEffect(state => brancher(using state))
  
  def sequence(effects: HistoryState ?=> Iterable[Effect]): Effect =
    Effect(effects.foldLeft(Effect.identity)(_ |> _))
  
  def endTurn: Effect = EndTurnEffect
  
  def stop(outcome: HistoryState ?=> Outcome): Rule =
    TerminalEffect(state => outcome(using state))
  
  def create (
    owner: HistoryState ?=> PlayerId,
    region: HistoryState ?=> HasRegionI,
    pieceTypes: PieceType*,
  ): Effect =
    Effect(CreateEffect(owner, region.region, pieceTypes.toIndexedSeq))
  
  def createMine (
    region: HistoryState ?=> HasRegionI,
    pieceTypes: PieceType*,
  ) (using owner: PlayerId): Effect =
    Effect.create(owner, region, pieceTypes*)
  
  def relocate (
    pieces: HistoryState ?=> PieceFilter,
    position: (HistoryState, Piece) ?=> HasVecI,
  ): Effect = Effect.sequence:
    pieces.now.map: piece =>
      Effect(MoveEffect(piece, position(using summon[HistoryState], piece).position))
      
  def slide (
    from: HistoryState ?=> HasRegionI,
    to: (HistoryState, VecI) ?=> HasVecI,
  ): Effect =
    relocate (
      State.pieces.ofRegion(from),
      to(using summon[HistoryState], summon[Piece].position),
    )
    
  /*def moveRelative (
    pieces: HistoryState ?=> HasPieceFilter,
    direction: HistoryState ?=> HasVecI,
  ): Effect =
    Effect.sequence:
      pieces.pieceFilter.pieceIds.map: piece =>
        Effect.move(piece, piece.now.get + direction)*/
    
  def destroy (
    pieces: HistoryState ?=> PieceFilter,
  ): Effect =
    Effect(DestroyEffect(pieces.now))
    
  def clear (
    region: HistoryState ?=> HasRegionI,
  ): Effect =
    Effect.destroy(Pieces.now.ofRegion(region))
  
  private[rules] case object IdentityEffect extends Effect:
    
    def effect(state: HistoryState) = Some:
      state.withRule(Effect.identity)
    
  private[rules] case class SequenceEffect (
    left: Effect,
    right: Effect,
  ) extends Effect:
    
    def effect(state: HistoryState) =
      right.effect(left.effect(state).value.history)
  
  private[rules] sealed trait PieceEffect extends Effect:
    
    final def effect(state: HistoryState) = Some:
      state.replace(state.now.withPieces(effect(state.now.pieces)))
        .withRule(Effect.identity)
    
    def effect(pieces: PieceState): PieceState
  
  private[rules] case class CreateEffect (
    owner: PlayerId,
    region: RegionI,
    pieceTypes: IndexedSeq[PieceType],
  ) extends PieceEffect:
    
    def effect(pieceSet: PieceState) =
      if pieceTypes.isEmpty then pieceSet else
        (region & pieceSet.board).positions.zipWithIndex.foldLeft(pieceSet):
          case (pieceSet, (pos, i)) =>
            val pieceType = pieceTypes(i % pieceTypes.size)
            val result = pieceSet.createPiece(pieceType, pos, owner)
            result
  
  private[rules] case class MoveEffect (
    piece: PieceRef,
    position: VecI,
  ) extends PieceEffect:
    
    def effect(pieceSet: PieceState) =
      pieceSet.movePiece(piece, position)
  
  private[rules] case class DestroyEffect (
    pieces: PieceSet,
  ) extends PieceEffect:
    
    def effect(pieceSet: PieceState) =
      pieces.pieceRefs.foldLeft(pieceSet)(_.destroyPiece(_))
  
  private[rules] case object EndTurnEffect extends Effect:
    
    def effect(state: HistoryState) =
      Some(state.replace(state.now.endTurn).withRule(Effect.identity))
  
  /**
   * A rule which immediately triggers the end of the game.
   *
   * @param outcome the outcome of the game, i.e. who won?
   */
  private[rules] case class TerminalEffect (
    outcome: HistoryState => Outcome,
  ) extends Effect:
      
    def effect(state: HistoryState) = Some:
      state.withOutcome(outcome(state))
      
  private[rules] case class SwitchEffect (
    brancher: HistoryState => Effect,
  ) extends Effect:
    
    private val memo: mutable.Map[HistoryState, Effect] = mutable.Map.empty
    private def branch(state: HistoryState): Effect =
      memo.getOrElseUpdate(state, brancher(state))
    
    def effect(state: HistoryState) =
      branch(state).effect(state)