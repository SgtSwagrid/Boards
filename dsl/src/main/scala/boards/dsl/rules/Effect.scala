package boards.dsl.rules

import boards.dsl.meta.Game.Board
import boards.dsl.meta.PlayerRef.{PlayerId, PlayerRef}
import boards.dsl.pieces.{Piece, PieceFilter, PieceRef, PieceSet, PieceState, PieceType}
import boards.dsl.states.GameState.Outcome
import boards.dsl.states.{GameState, HistoryState}
import boards.math.vector.Region.RegionI
import boards.math.vector.Vec.VecI
import boards.dsl.rules.Effect.*

import scala.collection.mutable

/** A [[Rule]] for modifying the [[InstantaneousState]] in some regular manner.
  * All state updates must occur via an [[Effect]].
  */
sealed trait Effect extends Rule:
  
  private[dsl] final def successors(state: HistoryState) =
    LazyList.from(effect(state))
    
  private[dsl] override def effect(state: HistoryState): Some[GameState]
  
  final def |> (that: => Effect): Effect =
    if this != Effect.identity
    then SequenceEffect(this, that)
    else that

object Effect:
  
  /** An [[Effect]] which does nothing.
    * Acts as the identity [[Rule]] under the [[|>]] operator.
    *
    * @see [[Cause.none]]
    */
  val identity: Effect = IdentityEffect
  
  def apply (brancher: (state: HistoryState) ?=> Effect): Effect =
    SwitchEffect(state => brancher(using state))
  
  def sequence (effects: (state: HistoryState) ?=> Iterable[Effect]): Effect =
    Effect(effects.foldLeft(Effect.identity)(_ |> _))
  
  def endTurn: Effect = EndTurnEffect
  
  def stop (outcome: HistoryState ?=> Outcome): Effect =
    TerminalEffect(state => outcome(using state))
  
  def stopWhen
    (condition: (state: HistoryState) ?=> Boolean)
    (outcome: (state: HistoryState) ?=> Outcome)
  : Effect =
    Effect(if condition then Effect.stop(outcome) else Effect.identity)
  
  def create (
    owner: (state: HistoryState) ?=> PlayerRef,
    region: (state: HistoryState) ?=> RegionI,
    pieceTypes: PieceType*,
  ): Effect =
    Effect(CreateEffect(owner, region, pieceTypes.toIndexedSeq))
  
  def createFriendly (
    region: (state: HistoryState) ?=> RegionI,
    pieceTypes: PieceType*,
  ) (using owner: PlayerRef): Effect =
    Effect.create(owner, region, pieceTypes*)
    
  def createNeutral (
    region: (state: HistoryState) ?=> RegionI,
    pieceTypes: PieceType*,
  ): Effect =
    Effect.create(PlayerId(-1), region, pieceTypes*)
  
  def relocate (
    pieces: (state: HistoryState) ?=> PieceFilter,
    position: (state: HistoryState, piece: Piece) ?=> VecI,
  ): Effect = Effect.sequence:
    pieces.now.map: piece =>
      Effect(MoveEffect(piece, position(using summon[HistoryState], piece).position))
      
  def slide (
    from: (state: HistoryState) ?=> RegionI,
    to: (state: HistoryState, from: VecI) ?=> VecI,
  ): Effect =
    relocate (
      state.pieces.ofRegion(from),
      to(using summon[HistoryState], summon[Piece].position),
    )
    
  def destroy (
    pieces: (state: HistoryState) ?=> PieceFilter,
  ): Effect =
    Effect(DestroyEffect(state.pieces))
    
  def clear (
    region: (state: HistoryState) ?=> RegionI,
  ): Effect =
    Effect.destroy(state.pieces.ofRegion(region))
    
  def setBoard (
    board: (state: HistoryState) ?=> Board,
  ): Effect =
    Effect(BoardEffect(board))
  
  private[rules] case object IdentityEffect extends Effect:
    
    def effect (state: HistoryState) = Some:
      state.withRule(Effect.identity)
    
  private[rules] case class SequenceEffect (
    left: Effect,
    right: Effect,
  ) extends Effect:
    
    def effect (state: HistoryState) =
      right.effect(left.effect(state).value.history)
  
  private[rules] case class BoardEffect (
    board: Board,
  ) extends Effect:
    
    final def effect (state: HistoryState) = Some:
      state.replace(state.now.withBoard(board))
        .withRule(Effect.identity)
  
  private[rules] sealed trait PieceEffect extends Effect:
    
    final def effect (state: HistoryState) = Some:
      state.replace(state.now.withPieces(effect(state.now.pieces)))
        .withRule(Effect.identity)
    
    def effect (pieces: PieceState): PieceState
  
  private[rules] case class CreateEffect (
    owner: PlayerRef,
    region: RegionI,
    pieceTypes: IndexedSeq[PieceType],
  ) extends PieceEffect:
    
    def effect (pieceSet: PieceState) =
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
    
    def effect (pieceSet: PieceState) =
      pieceSet.movePiece(piece, position)
  
  private[rules] case class DestroyEffect (
    pieces: PieceFilter,
  ) extends PieceEffect:
    
    def effect (pieceSet: PieceState) =
      pieceSet.filter(pieces).pieces.foldLeft(pieceSet)(_.destroyPiece(_))
  
  private[rules] case object EndTurnEffect extends Effect:
    
    def effect (state: HistoryState) =
      Some(state.replace(state.now.endTurn).withRule(Effect.identity))
  
  /** A rule which immediately triggers the end of the game.
    * @param outcome the outcome of the game, i.e. who won?
    */
  private[rules] case class TerminalEffect (
    outcome: HistoryState => Outcome,
  ) extends Effect:
      
    def effect (state: HistoryState) = Some:
      state.withOutcome(outcome(state))
      
  private[rules] case class SwitchEffect (
    brancher: HistoryState => Effect,
  ) extends Effect:
    
    private val memo: mutable.Map[HistoryState, Effect] = mutable.Map.empty
    private def branch (state: HistoryState): Effect =
      memo.getOrElseUpdate(state, brancher(state))
    
    def effect (state: HistoryState) =
      branch(state).effect(state)