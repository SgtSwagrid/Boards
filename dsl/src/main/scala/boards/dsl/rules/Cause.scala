package boards.dsl.rules

import boards.dsl.pieces.{Piece, PieceFilter}
import boards.dsl.rules.Cause.{FilterCause, UnionCause}
import boards.dsl.states.{GameState, HistoryState}
import boards.math.vector.Region.RegionI
import boards.math.vector.Vec.VecI

import scala.collection.mutable

/** A [[Rule]] for enumerating all [[Input]]s of a particular kind,
  * to indicate that these [[Input]]s will be accepted as legal.
  *
  * Produces a unique successor state for each applicable [[Input]].
  *
  * Along with [[Effect]]s, [[Cause]]s form the leaves of the [[Rule]] tree.
  *
  * [[Cause]]s are the standard way to block execution until an [[Input]] is received,
  * and to control execution so that only certain branches are followed.
  */
trait Cause extends Rule:
  
  private[dsl] final def successors(state: HistoryState): LazyList[GameState] =
    inputs(state).map(state.withCause).map(_.withRule(Effect.identity))
  
  private[dsl] final def effect(state: HistoryState): None.type = None
  
  /** @see [[Rule.union]] */
  final def | (that: => Cause): Cause =
    UnionCause(this, that)
  
  /** Allow only some subset of the [[Input]]s. */
  final def filter(condition: Input => Boolean): Cause =
    FilterCause(this, condition)
  
  /** Enumerates all legal [[Input]]s from the given state. */
  def inputs(state: HistoryState): LazyList[Input]

object Cause:
  
  /** Accepts no [[Input]]s, blocking all execution.
    * Acts as the identity [[Rule]] under the [[|]] operator.
    *
    * @see [[Effect.identity]]
    */
  val none: Cause = EmptyCause
  
  /** @see [[Rule.apply]] */
  def apply (brancher: (state: HistoryState) ?=> Cause): Cause =
    SwitchCause(state => brancher(using state))
  
  /** @see [[Rule.union]] */
  def union (causes: (state: HistoryState) ?=> Iterable[Cause]): Cause =
    Cause(causes.foldLeft(Cause.none)(_ | _))
  
  /** Accepts a single [[Input]] corresponding to a [[Click]] of a specific position. */
  def click (
    region: (state: HistoryState) ?=> RegionI,
  ): Cause =
    Cause(ClickAny(region))
  
  /** Accepts a single [[Input]] corresponding to a [[Click]] of the entire `region` as a single unit. */
  def clickRegion (
    region: (state: HistoryState) ?=> RegionI,
  ): Cause =
    Cause(ClickAll(region))
  
  /** Accepts any [[Input]] corresponding to a click of any of the given [[Piece]]s. */
  def clickPiece (
    pieces: (state: HistoryState) ?=> PieceFilter,
  ): Cause =
    Cause.click(pieces.now.region)
  
  /** Accepts any [[Input]] corresponding to a [[Drag]] from anywhere in `from` to anywhere in `to`. */
  def drag (
    from: (state: HistoryState) ?=> RegionI,
    to: (state: HistoryState, from: VecI) ?=> RegionI,
  ): Cause = Cause.union:
    from.positions.map: position =>
      DragAny(from, to(using summon[HistoryState], position))
  
  /** Accepts all [[Input]]s corresponding to a [[Drag]] from anywhere in `from` to the corresponding place in `to`.
    * Here, corresponding means at the same index in the respective `Region`.
    */
  def dragCorresponding (
    from: (state: HistoryState) ?=> RegionI,
    to: (state: HistoryState) ?=> RegionI,
  ): Cause =
    Cause(DragCorresponding(from, to))
  
  /** Accepts a single [[Input]] corresponding to a [[Drag]]
    * from the entire region `from` to the entire region `to`.
    */
  def dragRegion (
    from: (state: HistoryState) ?=> RegionI,
    to: (state: HistoryState) ?=> RegionI,
  ): Cause =
    Cause(DragAll(from, to))
  
  /** Accepts any [[Input]] corresponding to a [[Drag]] of any [[Piece]] in `pieces` to anywhere in `region`. */
  def dragPiece (
    pieces: (state: HistoryState) ?=> PieceFilter,
    region: (state: HistoryState, piece: Piece) ?=> RegionI,
  ): Cause = Cause.union:
    pieces.now.map: piece =>
      Cause.drag(piece.region, region(using summon[HistoryState], piece))
  
  /** Any [[Cause]] related to a [[Click]] of the [[Board]]. */
  sealed trait ClickCause extends Cause
  
  private[rules] case class ClickAll (
    region: RegionI,
  ) extends ClickCause:
    
    def inputs (state: HistoryState): LazyList[Input.Click] =
      val region = this.region & state.board
      if region.positions.nonEmpty then LazyList(Input.click(region)) else LazyList.empty
  
  private[rules] case class ClickAny (
    region: RegionI,
  ) extends ClickCause:
    
    def inputs (state: HistoryState): LazyList[Input.Click] =
      (region & state.board).positions.map(Input.click)
  
  /** Any [[Cause]] related to a [[Drag]] over the [[Board]]. */
  sealed trait DragCause extends Cause
  
  case class DragAll (
    from: RegionI,
    to: RegionI,
  ) extends DragCause:
    
    def inputs (state: HistoryState): LazyList[Input.Drag] =
      val from = this.from & state.board
      val to = this.to & state.board
      if from.positions.nonEmpty && to.positions.nonEmpty
      then LazyList(Input.drag(from, to)) else LazyList.empty
  
  private[rules] case class DragAny (
    from: RegionI,
    to: RegionI,
  ) extends DragCause:
    
    def inputs (state: HistoryState): LazyList[Input.Drag] = for
      from <- (from & state.board).positions
      to <- (to & state.board).positions
    yield Input.drag(from, to)
  
  private[rules] case class DragCorresponding (
    from: RegionI,
    to: RegionI,
  ) extends DragCause:
    
    def inputs (state: HistoryState): LazyList[Input.Drag] =
      ((from & state.board).positions zip (to & state.board).positions)
        .map(Input.drag)
  
  private[rules] case object EmptyCause extends Cause:
    
    def inputs (state: HistoryState): LazyList[Nothing] =
      LazyList.empty
  
  private[rules] case class UnionCause (
    left: Cause,
    right: Cause,
  ) extends Cause:
    
    def inputs (state: HistoryState): LazyList[Input] =
      (left.inputs(state) ++ right.inputs(state)).distinct
      
  private[rules] case class SwitchCause (
    brancher: HistoryState => Cause,
  ) extends Cause:
    
    private val memo: mutable.Map[HistoryState, Cause] = mutable.Map.empty
    private def branch (state: HistoryState): Cause =
      memo.getOrElseUpdate(state, brancher(state))
    
    def inputs (state: HistoryState): LazyList[Input] =
      branch(state).inputs(state)
      
  private[rules] case class FilterCause (
    base: Cause,
    condition: Input => Boolean,
  ) extends Cause:
    
    def inputs (state: HistoryState): LazyList[Input] =
      base.inputs(state).filter(condition)