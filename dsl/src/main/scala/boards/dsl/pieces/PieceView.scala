package boards.dsl.pieces

import boards.dsl.meta.PlayerRef.{PlayerId, PlayerRef}
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

/** A set of [[Piece]]s at a specific point in time.
  * Contains all relevant [[Piece]] information from this time.
  *
  * Produced by filtering a [[PieceState]] to analyse some subset of the [[Piece]]s.
  *
  * This instance will not remain up-to-date with the latest changes.
  * Call [[this.now]] to query the latest version of the [[Piece]]s in this [[PieceView]].
  *
  * @see [[PieceSet]], [[PieceState]]
  */
trait PieceView extends PieceSet, UpdateQuery, OfPlayer[PieceView], HasRegionI:
  
  /** The version of the [[PieceSet]] from which this [[PieceView]] is derived. */
  val version: Version
  
  /** The set of `pieceId`s for all of the [[Pieces]] in this [[PieceView]]. */
  val selected: PieceSet
  
  /** The set of all currently existing [[Piece]]s in this [[PieceView]]. */
  val pieces: LazyList[Piece]
  
  /** The [[Piece]] at the specified position, if there is one. */
  def at(position: HasVecI): Option[Piece]
  
  /** Get the latest version of the [[Piece]] corresponding to the given [[PieceRef]], if it still exists. */
  def get(id: PieceRef): Option[Piece]
  
  /** Get a [[PieceView]] of this [[PieceState]] with only some specified subset of the pieces. */
  def restrictTo(pieces: PieceSet): PieceView
  
  export pieces.map
  
  protected[pieces] final val pieceBitset: BitSet =
    selected.pieceBitset
  
  /** Whether some [[Piece]] exists at the given position. */
  final def contains(position: HasVecI): Boolean =
    at(position).isDefined
  
  /** Whether some [[Piece]] belonging to the current player exists at the given position. */
  final def isFriendly(position: HasVecI)(using player: PlayerRef): Boolean =
    at(position).exists(_.owner == player)
  
  /** Whether some [[Piece]] belonging to another player exists at the given position. */
  final def isEnemy(position: HasVecI)(using player: PlayerRef): Boolean =
    at(position).exists(_.owner != player)
  
  /** The [[RegionI]] describing the positions of all included [[Piece]]s. */
  final def region: RegionI =
    Region.from(pieces.map(_.position))
  
  /** Restrict the [[PieceView]] to only those [[Piece]]s satisfying some predicate. */
  final def filter(filter: PieceFilter): PieceView =
    filter.apply(this)
  
  /** Restrict the [[PieceView]] to only those [[Piece]]s belonging to the specified player. */
  final override def ofPlayer(players: PlayerRef*): PieceView =
    filter(PieceFilter.ofPlayer(players*))
  
  /** Restrict the [[PieceView]] to only those [[Piece]]s of a particular [[PieceType]]. */
  final override def ofType(pieceTypes: PieceType*): PieceView =
    filter(PieceFilter.ofType(pieceTypes*))
  
  /** Restrict the [[PieceView]] to only those [[Piece]]s of a particular [[PieceType]] class. */
  final override def ofClass(pieceTypes: Class[? <: PieceType]*): PieceView =
    filter(PieceFilter.ofClass(pieceTypes*))
  
  /** Restrict the [[PieceView]] to only those [[Piece]]s of a particular [[PieceType]] class. */
  final override def ofClass[P <: PieceType](using C: ClassTag[P]): PieceView =
    ofClass(C.runtimeClass.asInstanceOf[Class[? <: PieceType]])
  
  /** Restrict the [[PieceView]] to only those [[Piece]]s in a particular [[RegionI]]. */
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