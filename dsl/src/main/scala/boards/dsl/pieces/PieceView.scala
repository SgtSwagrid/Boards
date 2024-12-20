package boards.dsl.pieces

import boards.dsl.meta.PlayerId.PlayerId
import boards.dsl.pieces.PieceState.Version
import boards.dsl.pieces.PieceRef
import boards.dsl.pieces.PieceUpdate.UpdateQuery

import scala.collection.immutable.BitSet
import boards.dsl.pieces.{PieceFilter, PieceSet}
import boards.dsl.rules.Rule
import boards.dsl.states.{HistoryState, InstantaneousState}
import boards.dsl.states.HistoryState.AtTime
import boards.math.region.Region.{HasRegionI, RegionI}
import boards.math.region.Vec.HasVecI
import boards.math.region.Region

import scala.reflect.ClassTag

trait PieceView extends PieceSet, UpdateQuery, OfPlayer[PieceView], HasRegionI:
  
  val version: Version
  val selected: PieceSet
  val pieces: LazyList[Piece]
  def at(position: HasVecI): Option[Piece]
  def get(id: PieceRef): Option[Piece]
  def restrictTo(pieces: PieceSet): PieceView
  
  export pieces.map
  
  protected[pieces] final val pieceBitset: BitSet =
    selected.pieceBitset
  
  final def contains(position: HasVecI): Boolean =
    at(position).isDefined
  
  final def isFriendly(position: HasVecI)(using playerId: PlayerId): Boolean =
    at(position).exists(_.owner == playerId)
  
  final def isEnemy(position: HasVecI)(using playerId: PlayerId): Boolean =
    at(position).exists(_.owner != playerId)
  
  final def region: RegionI =
    Region.from(pieces.map(_.position))
  
  final def filter(filter: PieceFilter): PieceView =
    filter.apply(this)
  
  final override def ofPlayer(playerIds: PlayerId*): PieceView =
    filter(PieceFilter.ofPlayer(playerIds*))
  
  final override def ofType(pieceTypes: PieceType*): PieceView =
    filter(PieceFilter.ofType(pieceTypes*))
  
  final override def ofClass(pieceTypes: Class[? <: PieceType]*): PieceView =
    filter(PieceFilter.ofClass(pieceTypes*))
  
  final override def ofClass[P <: PieceType](using C: ClassTag[P]): PieceView =
    ofClass(C.runtimeClass.asInstanceOf[Class[? <: PieceType]])
  
  final override def ofRegion(region: HasRegionI): PieceView =
    filter(PieceFilter.ofRegion(region))
  
  override def toString = pieces.size match
    case 0 => "∅"
    case 1 => pieces.head.toString
    case _ => pieces.mkString("{", ", ", "}")
    
object PieceView:
  
  def empty: PieceView = FilteredPieceView()
  def from(base: PieceState, selected: PieceSet): PieceView =
    FilteredPieceView(base, base & selected)
  
  case class FilteredPieceView (
    base: PieceState = PieceState.empty,
    override val selected: PieceSet = PieceSet.empty,
  ) extends PieceView:
  
    val version: Version = base.version
    
    val pieces: LazyList[Piece] =
      pieceRefs.map(_.pieceId).map(base.piecesById)
      
    def at(position: HasVecI): Option[Piece] =
      base.piecesByPos.get(position.position)
        .map(base.piecesById)
        .filter(this.contains)
      
    def get(pieceRef: PieceRef): Option[Piece] =
      base.piecesById.get(pieceRef.pieceId).filter(this.contains)
      
    val updates: LazyList[PieceUpdate] =
      LazyList.from(base.updates).filter(contains)
    
    def restrictTo(pieces: PieceSet): PieceView =
      PieceView.from(base, selected & pieces)
  
  object Pieces extends AtTime[PieceState]:
    def atTime(time: HistoryState): PieceState = time.pieces
    export PieceFilter.*