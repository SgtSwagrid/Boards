package boards.dsl.pieces

import boards.dsl.pieces.{PieceSet, PieceState, PieceView}
import PieceFilter.*
import boards.dsl.meta.PlayerId.PlayerId
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
import boards.dsl.shortcuts.{*, given}
import boards.dsl.pieces.PieceView.FilteredPieceView

import scala.reflect.ClassTag

trait PieceFilter extends AtTime[PieceView], PeriodQuery[UpdateQuery], OfPlayer[PieceFilter]:
  
  protected def applyBase(pieces: PieceState): PieceView
  
  final def apply(pieces: PieceView): PieceView = pieces match
    case pieces: PieceState => applyBase(pieces)
    case FilteredPieceView(base, selected) => applyBase(base).restrictTo(selected)

  final def | (that: PieceFilter): PieceFilter =
    UnionFilter(this, that)
  
  final def & (that: PieceFilter): PieceFilter =
    IntersectionFilter(this, that)
  
  final def \ (that: PieceFilter): PieceFilter =
    DifferenceFilter(this, that)
  
  final def ^ (that: PieceFilter): PieceFilter =
    SymmetricDifferenceFilter(this, that)
    
  def ofPlayer(playerIds: PlayerId*): PieceFilter =
    this & PieceFilter.ofPlayer(playerIds*)
    
  def ofType(pieceTypes: PieceType*): PieceFilter =
    this & PieceFilter.ofType(pieceTypes*)
  
  def ofClass(pieceTypes: Class[? <: PieceType]*): PieceFilter =
    this & PieceFilter.ofClass(pieceTypes*)
  
  def ofClass [P <: PieceType] (using C: ClassTag[P]): PieceFilter =
    this & PieceFilter.ofClass[P]
    
  def ofRegion(region: HasRegionI): PieceFilter =
    this & PieceFilter.ofRegion(region)
    
  def atTime(state: HistoryState): PieceView =
    state.pieces.filter(this)
  
  def between(start: HistoryState, end: HistoryState): UpdateQuery =
    UpdateQuery.of:
      atTime(end).updates.takeWhile(_.time.toInt >= start.version.toInt)
  
  final def actions: Rule =  Rule.union:
    now.map(_.actions)
    
  final def fromNow(using HistoryState): Capability =
    actions.fromNow
    
  final def following(effect: Effect)(using HistoryState): Capability =
    actions.from(effect.effect(summon[HistoryState]).get.history)
  
  final def inCheck(using state: HistoryState): Boolean =
    now.pieces.exists: piece =>
      Pieces.ofOtherPlayers(piece.owner).fromNow.canCapture(piece)
  
  def relocate (
    to: (HistoryState, Piece) ?=> HasVecI,
  ): Effect =
    Effect.relocate(this, to)
  
  def move (
    to: (HistoryState, Piece) ?=> HasRegionI,
  ): Rule =
    Control.move(this, to)
  
  def destroy: Effect =
    Effect.destroy(this)
  
  def capture: Rule =
    Control.capture(this)
  
  def promote (
    pieceTypes: PieceType*
  ): Rule =
    Control.promote(this, pieceTypes*)
  
  def click: Cause =
    Cause.clickPiece(this)
  
  def dragTo(region: HasRegionI): Cause =
    Cause.dragPiece(this, region)

object PieceFilter extends OfPlayer[PieceFilter]:
  
  val empty: PieceFilter = EmptyFilter
  val all: PieceFilter = UniversalFilter
  
  def ofPlayer(playerIds: PlayerId*): PieceFilter =
    PlayerFilter(playerIds*)
  
  def ofType(pieceTypes: PieceType*): PieceFilter =
    TypeFilter(pieceTypes*)
  
  def ofClass(pieceTypes: Class[? <: PieceType]*): PieceFilter =
    ClassFilter(pieceTypes*)
  
  def ofClass [P <: PieceType] (using C: ClassTag[P]): PieceFilter =
    ofClass(C.runtimeClass.asInstanceOf[Class[? <: PieceType]])
  
  def ofRegion(region: HasRegionI): PieceFilter =
    RegionFilter(region.region)
  
  private object EmptyFilter extends PieceFilter:
    def applyBase(pieces: PieceState): PieceView = PieceView.empty
    
  private object UniversalFilter extends PieceFilter:
    def applyBase(pieces: PieceState): PieceView = pieces
  
  private class PlayerFilter (
    playerIds: PlayerId*,
  ) extends PieceFilter:
    
    def applyBase(pieces: PieceState): PieceView =
      pieces.restrictTo:
        playerIds
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