package boards.dsl.rules

import boards.imports.games.{*, given}
import boards.imports.math.{*, given}
import Cause.*
import boards.dsl.pieces.{Piece, PieceFilter, PieceSet}
import boards.dsl.rules.Effect.SequenceEffect
import boards.math.region.Region.HasRegionI
import boards.math.region.Vec.HasVecI
import boards.dsl.shortcuts.{given_HistoryState, piece}
import boards.math.region.Ray

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * A `Rule` for enumerating all `Input`s of a particular kind,
 * to indicate that these `Input`s will be accepted as legal.
 * `Cause`s form the leaf nodes of the `Rule` tree.
 */
trait Cause extends Rule:
  
  private[dsl] final def successors(state: HistoryState): LazyList[GameState] =
    inputs(state).map(state.withCause).map(_.withRule(Effect.identity))
  
  private[dsl] final def effect(state: HistoryState): None.type = None
  
  final def | (that: => Cause): Cause =
    UnionCause(this, that)
  
  final def filter(condition: Input => Boolean): Cause =
    FilterCause(this, condition)
    
  def inputs(state: HistoryState): LazyList[Input]
  
  def structure(state: HistoryState) = "!"

object Cause:
  
  /** Accepts no `Input`s. */
  val none: Cause = EmptyCause
  
  def apply(brancher: HistoryState ?=> Cause): Cause =
    SwitchCause(state => brancher(using state))
  
  def union(causes: HistoryState ?=> Iterable[Cause]): Cause =
    Cause(causes.foldLeft(Cause.none)(_ | _))
  
  /** Accepts a single `Input` corresponding to a `Click` of a specific position. */
  def click (
    region: HistoryState ?=> HasRegionI,
  ): Cause =
    Cause(ClickAny(region.region))
  
  /**
   * Accepts a single `Input` corresponding to a `Click` of the entire `region` as a single unit.
   */
  def clickRegion (
    region: HistoryState ?=> HasRegionI,
  ): Cause =
    Cause(ClickAll(region.region))
    
  def clickPiece (
    pieces: HistoryState ?=> PieceFilter,
  ): Cause =
    Cause.click(pieces.now.region)
  
  def drag (
    from: HistoryState ?=> HasRegionI,
    to: (HistoryState, VecI) ?=> HasRegionI,
  ): Cause = Cause.union:
    from.region.positions.map: position =>
      DragAny(from.region, to(using summon[HistoryState], position).region)
  
  /**
   * Accepts all `Inputs` corresponding to a `Drag` from anywhere in `from` to the corresponding place in `to`.
   * Here, corresponding means at the same index in the respective `Region`.
   */
  def dragCorresponding (
    from: HistoryState ?=> HasRegionI,
    to: HistoryState ?=> HasRegionI,
  ): Cause =
    Cause(DragCorresponding(from.region, to.region))
  
  /**
   * Accepts a single `Input` corresponding to a `Drag`
   * from the entire region `from` to the entire region `to`.
   */
  def dragRegion (
    from: HistoryState ?=> HasRegionI,
    to: HistoryState ?=> HasRegionI,
  ): Cause =
    Cause(DragAll(from.region, to.region))
    
  def dragPiece (
    pieces: HistoryState ?=> PieceFilter,
    region: (HistoryState, Piece) ?=> HasRegionI,
  ): Cause = Cause.union:
    pieces.now.map: piece =>
      Cause.drag(piece, region(using summon[HistoryState], piece))
  
  sealed trait ClickCause extends Cause
  
  private[rules] case class ClickAll (
    region: RegionI,
  ) extends ClickCause:
    
    def inputs(state: HistoryState): LazyList[Input.Click] =
      val region = this.region & state.board
      if region.positions.nonEmpty then LazyList(Input.click(region)) else LazyList.empty
  
  private[rules] case class ClickAny (
    region: RegionI,
  ) extends ClickCause:
    
    def inputs(state: HistoryState): LazyList[Input.Click] =
      (region & state.board).positions.map(Input.click)
  
  sealed trait DragCause extends Cause
  
  case class DragAll (
    from: RegionI,
    to: RegionI,
  ) extends DragCause:
    
    def inputs(state: HistoryState): LazyList[Input.Drag] =
      val from = this.from & state.board
      val to = this.to & state.board
      if from.positions.nonEmpty && to.positions.nonEmpty
      then LazyList(Input.drag(from, to)) else LazyList.empty
  
  private[rules] case class DragAny (
    from: RegionI,
    to: RegionI,
  ) extends DragCause:
    
    def inputs(state: HistoryState): LazyList[Input.Drag] = for
      from <- (from & state.board).positions
      to <- (to & state.board).positions
    yield Input.drag(from, to)
  
  private[rules] case class DragCorresponding (
    from: RegionI,
    to: RegionI,
  ) extends DragCause:
    
    def inputs(state: HistoryState): LazyList[Input.Drag] =
      (from & state.board).positions
        .zip((to & state.board).positions)
        .map(Input.drag)
  
  private[rules] case object EmptyCause extends Cause:
    
    def inputs(state: HistoryState): LazyList[Nothing] =
      LazyList.empty
  
  private[rules] case class UnionCause (
    left: Cause,
    right: Cause,
  ) extends Cause:
    
    def inputs(state: HistoryState): LazyList[Input] =
      (left.inputs(state) ++ right.inputs(state)).distinct
      
  private[rules] case class SwitchCause (
    brancher: HistoryState => Cause,
  ) extends Cause:
    
    private val memo: mutable.Map[HistoryState, Cause] = mutable.Map.empty
    private def branch(state: HistoryState): Cause =
      memo.getOrElseUpdate(state, brancher(state))
    
    def inputs(state: HistoryState): LazyList[Input] =
      branch(state).inputs(state)
      
  private[rules] case class FilterCause (
    base: Cause,
    condition: Input => Boolean,
  ) extends Cause:
    
    def inputs(state: HistoryState): LazyList[Input] =
      base.inputs(state).filter(condition)