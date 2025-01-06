package boards.dsl.pieces

import boards.dsl.pieces.PieceRef.PieceId
import boards.dsl.pieces.{Piece, PieceRef, PieceSet, PieceState, PieceView}
import boards.dsl.rules.{Control, Effect, Rule}
import boards.dsl.states.HistoryState
import boards.math.region.Region.HasRegionI
import boards.math.region.Vec.HasVecI

import scala.collection.immutable.BitSet

/** A set of [[PieceId]]s without any time-varying information.
  * Can identity some subset of existing [[Piece]]s, but contains no
  * information about their current state.
  *
  * Call [[this.now]] to query the current state of the included [[Piece]]s.
  *
  * @see [[PieceFilter]], [[PieceView]]
  *
  * @author Alec Dorrington
  */
trait PieceSet extends PieceFilter:
  
  protected def applyBase(pieces: PieceState): PieceView =
    pieces.restrictTo(this)
  
  /** A [[BitSet]] describing the IDs of all included pieces. */
  protected[pieces] val pieceBitset: BitSet
  
  export pieceBitset.{isEmpty, nonEmpty, size, sizeIs}
  
  final def pieceRefs: LazyList[PieceRef] =
    LazyList.from(pieceBitset.iterator.map(PieceRef.apply))
    
  final def contains(piece: PieceRef): Boolean =
    pieceBitset.contains(piece.pieceId.toInt)
  
  /** Take the union of two [[PieceSet]]s. */
  final def | (that: PieceSet): PieceSet =
    PieceSet.apply(pieceBitset | that.pieceBitset)
  
  /** Take the intersection of two [[PieceSet]]s. */
  final def & (that: PieceSet): PieceSet =
    PieceSet.apply(pieceBitset & that.pieceBitset)
  
  /** Take the set difference between two [[PieceSet]]s. */
  final def \ (that: PieceSet): PieceSet =
    PieceSet.apply(pieceBitset -- that.pieceBitset)
  
  /** Take the symmetric difference between two [[PieceSet]]s. */
  final def ^ (that: PieceSet): PieceSet =
    PieceSet.apply(pieceBitset ^ that.pieceBitset)

object PieceSet:
  
  /** A PieceSet containing no [[Piece]]s. */
  val empty: PieceSet = PieceSet()
  
  /** Construct a [[PieceSet]] consisting of the given [[Piece]]s. */
  def apply(pieces: PieceRef*): PieceSet =
    PieceSet.Of(BitSet(pieces.map(_.pieceId.toInt)*))
    
  /** Construct a [[PieceSet]] based on a [[BitSet]]. */
  def apply(pieceIds: BitSet): PieceSet =
    PieceSet.Of(pieceIds)
  
  private[pieces] case class Of (
    pieceBitset: BitSet = BitSet.empty,
  ) extends PieceSet