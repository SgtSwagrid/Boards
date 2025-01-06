package boards.dsl.pieces

import boards.dsl.pieces.{PieceSet, PieceState, PieceView}
import PieceFilter.*
import boards.dsl.meta.PlayerRef
import boards.dsl.pieces
import boards.dsl.pieces.PieceState.empty.region
import boards.dsl.pieces.PieceType
import boards.dsl.pieces.PieceUpdate.UpdateQuery
import boards.dsl.rules.{Capability, Control, Effect, Rule}
import boards.dsl.states.HistoryState.{AtTime, PeriodQuery}
import boards.dsl.states.{HistoryState, InstantaneousState}
import boards.math.region.Region.{HasRegionI, RegionI}
import boards.math.region.Vec.HasVecI
import boards.imports.games.{*, given}
import boards.imports.math.{*, given}
import boards.dsl.Shortcuts.{*, given}
import boards.dsl.meta.PlayerRef.PlayerRef
import boards.dsl.pieces.PieceView.FilteredPieceView

import scala.reflect.ClassTag

/** A [[Piece]] predicate, which can be used to filter [[Piece]]s and perform queries.
  *
  * @see [[PieceSet]]
  *
  * @author Alec Dorrington
  */
trait PieceFilter extends AtTime[PieceView], PeriodQuery[UpdateQuery], OfPlayer[PieceFilter]:
  
  /** Restrict a [[PieceView]] to only [[Piece]]s which satisfy this predicate. */
  final def apply(pieces: PieceView): PieceView = pieces match
    case pieces: PieceState => applyBase(pieces)
    case FilteredPieceView(base, selected) => applyBase(base).restrictTo(selected)
    
  /** A version of [[apply]] which only works for [[PieceView]]s which are also [[PieceState]]s.
    * This is slightly easier to do so this method should be the one overridden.
    */
  protected def applyBase(pieces: PieceState): PieceView
  
  /** Take the logical OR of two [[PieceFilter]]s, i.e. [[Piece]]s only need to satisfy one [[PieceFilter]]. */
  final def | (that: PieceFilter): PieceFilter =
    UnionFilter(this, that)
  
  /** Take the logical AND of two [[PieceFilter]]s, i.e. [[Piece]]s need to satisfy both [[PieceFilter]]s. */
  final def & (that: PieceFilter): PieceFilter =
    IntersectionFilter(this, that)
  
  /** Take the [[PieceFilter]] which accepts [[Piece]]s that are both accepted by the first operand and rejected by the second. */
  final def \ (that: PieceFilter): PieceFilter =
    DifferenceFilter(this, that)
  
  /** Take the logical XOR of two [[PieceFilter]]s. */
  final def ^ (that: PieceFilter): PieceFilter =
    SymmetricDifferenceFilter(this, that)
    
  /** Require additionally that [[Piece]]s belong to the given player. */
  def ofPlayer(players: PlayerRef*): PieceFilter =
    this & PieceFilter.ofPlayer(players*)
    
  /** Require additionally that [[Piece]]s have the given [[PieceType]]. */
  def ofType(pieceTypes: PieceType*): PieceFilter =
    this & PieceFilter.ofType(pieceTypes*)
  
  /** Require additionally that [[Piece]]s have the given [[PieceType]] class. */
  def ofClass(pieceTypes: Class[? <: PieceType]*): PieceFilter =
    this & PieceFilter.ofClass(pieceTypes*)
  
  /** Require additionally that [[Piece]]s have the given [[PieceType]] class. */
  def ofClass [P <: PieceType] (using C: ClassTag[P]): PieceFilter =
    this & PieceFilter.ofClass[P]
  
  /** Require additionally that [[Piece]]s lie in the given [[RegionI]]. */
  def ofRegion(region: HasRegionI): PieceFilter =
    this & PieceFilter.ofRegion(region)
    
  /** Get all [[Piece]]s which satisfied this predicate at the given time. */
  def atTime(state: HistoryState): PieceView =
    state.pieces.filter(this)
  
  /** Query the changes to the matching [[Piece]]s which occurred in the given period. */
  def between(start: HistoryState, end: HistoryState): UpdateQuery =
    UpdateQuery.of:
      atTime(end).updates.view.takeWhile(_.time.toInt >= start.version.toInt)
  
  /** Get [[Rule]] which enumerates actions that may be taken by matching [[Piece]]s. */
  final def actions: Rule =  Rule.union:
    now.map(_.actions)
    
  /** Determine what these [[Piece]]s can do in the current [[HistoryState]]. */
  final def fromNow(using HistoryState): Capability =
    actions.fromNow
    
  /** Determine what these [[Piece]]s ''could'' do following some hypothetical [[Effect]]. */
  final def following(effect: Effect)(using HistoryState): Capability =
    actions.from(effect.effect(summon[HistoryState]).get.history)
  
  /** Whether some matching [[Piece]] is currently in check, i.e. can be captured by an enemy [[Piece]]. */
  final def inCheck(using state: HistoryState): Boolean =
    now.pieces.exists: piece =>
      Pieces.ofOtherPlayers(piece.owner).fromNow.canCapture(piece)
  
  /** Passively move all matching [[Piece]]s elsewhere. */
  def relocate (
    to: (HistoryState, Piece) ?=> HasVecI,
  ): Effect =
    Effect.relocate(this, to)
  
  /** Allow the player to move some matching [[Piece]] elsewhere. */
  def move (
    to: (HistoryState, Piece) ?=> HasRegionI,
  ): Rule =
    Control.move(this, to)
  
  /** Remove this [[Piece]]. */
  def destroy: Effect =
    Effect.destroy(this)
  
  /** Allow the player to remove some matching [[Piece]] by clicking on it. */
  def capture: Rule =
    Control.capture(this)
  
  /** Allow the player to choose a new [[PieceType]] for some matching [[Piece]]. */
  def promote (
    pieceTypes: PieceType*
  ): Rule =
    Control.promote(this, pieceTypes*)

  /** Allow the player to click matching [[Piece]]s. */
  def click: Cause =
    Cause.clickPiece(this)
  
  /** Allow the player to drag matching [[Piece]]s. */
  def dragTo(region: HasRegionI): Cause =
    Cause.dragPiece(this, region)

object PieceFilter extends OfPlayer[PieceFilter]:
  
  /** A [[PieceFilter]] which accepts no [[Piece]]s. */
  val empty: PieceFilter = EmptyFilter
  
  /** A [[PieceFilter]] which accepts every [[Piece]]. */
  val all: PieceFilter = UniversalFilter
  
  /** A [[PieceFilter]] which accepts only [[Piece]]s belonging to (a) given player(s). */
  def ofPlayer(players: PlayerRef*): PieceFilter =
    PlayerFilter(players*)
  
  /** A [[PieceFilter]] which accepts only [[Piece]]s of a particular [[PieceType]]. */
  def ofType(pieceTypes: PieceType*): PieceFilter =
    TypeFilter(pieceTypes*)
  
  /** A [[PieceFilter]] which accepts only [[Piece]]s of a particular [[PieceType]] class. */
  def ofClass(pieceTypes: Class[? <: PieceType]*): PieceFilter =
    ClassFilter(pieceTypes*)
  
  /** A [[PieceFilter]] which accepts only [[Piece]]s of a particular [[PieceType]] class. */
  def ofClass [P <: PieceType] (using C: ClassTag[P]): PieceFilter =
    ofClass(C.runtimeClass.asInstanceOf[Class[? <: PieceType]])
  
  /** A [[PieceFilter]] which accepts only [[Piece]]s in a particular [[RegionI]]. */
  def ofRegion(region: HasRegionI): PieceFilter =
    RegionFilter(region.region)
  
  private object EmptyFilter extends PieceFilter:
    def applyBase(pieces: PieceState): PieceView = PieceView.empty
    
  private object UniversalFilter extends PieceFilter:
    def applyBase(pieces: PieceState): PieceView = pieces
  
  private class PlayerFilter (
    players: PlayerRef*,
  ) extends PieceFilter:
    
    def applyBase(pieces: PieceState): PieceView =
      pieces.restrictTo:
        players
          .map(_.playerId)
          .flatMap(pieces.piecesByOwner.get)
          .foldLeft(PieceSet.empty)(_ | _)
        
  private class TypeFilter (
    pieceTypes: PieceType*,
  ) extends PieceFilter:
    
    def applyBase(pieces: PieceState): PieceView =
      pieces.restrictTo:
        pieceTypes
          .flatMap(pieces.piecesByType.get)
          .foldLeft(PieceSet.empty)(_ | _)
        
  private class ClassFilter (
    pieceTypes: Class[? <: PieceType]*,
  ) extends PieceFilter:
    
    def applyBase(pieces: PieceState): PieceView =
      pieces.restrictTo:
        pieceTypes
          .flatMap(pieces.piecesByClass.get)
          .foldLeft(PieceSet.empty)(_ | _)
        
  private class RegionFilter (
    region: RegionI,
  ) extends PieceFilter:
    
    def applyBase(pieces: PieceState): PieceView =
      pieces.restrictTo:
        region.positions
          .flatMap(pieces.at)
          .foldLeft(PieceSet.empty)(_ | _)
  
  private class UnionFilter (
    left: PieceFilter,
    right: PieceFilter,
  ) extends PieceFilter:
    
    def applyBase(pieces: PieceState): PieceView =
      pieces.restrictTo(left(pieces) | right(pieces))
  
  private class IntersectionFilter (
    left: PieceFilter,
    right: PieceFilter,
  ) extends PieceFilter:
    
    def applyBase(pieces: PieceState): PieceView =
      pieces.restrictTo(left(pieces) & right(pieces))
  
  private class DifferenceFilter (
    left: PieceFilter,
    right: PieceFilter,
  ) extends PieceFilter:
    
    def applyBase(pieces: PieceState): PieceView =
      pieces.restrictTo(left(pieces) \ right(pieces))
  
  private class SymmetricDifferenceFilter (
    left: PieceFilter,
    right: PieceFilter,
  ) extends PieceFilter:
    
    def applyBase(pieces: PieceState): PieceView =
      pieces.restrictTo(left(pieces) ^ right(pieces))