package boards.dsl.pieces

import boards.dsl.pieces.PieceRef.PieceId
import boards.dsl.pieces.{Piece, PieceRef, PieceSet, PieceState, PieceView}
import boards.dsl.rules.{Control, Effect, Rule}
import boards.dsl.states.HistoryState
import boards.math.region.Region.HasRegionI
import boards.math.region.Vec.HasVecI

import scala.collection.immutable.BitSet

trait PieceSet extends PieceFilter:
  
  protected def applyBase(pieces: PieceState): PieceView =
    pieces.restrictTo(this)
    
  protected[pieces] val pieceBitset: BitSet
  
  export pieceBitset.{isEmpty, nonEmpty, size, sizeIs}
  
  final def pieceRefs: LazyList[PieceRef] =
    LazyList.from(pieceBitset.iterator.map(PieceRef.apply))
    
  final def contains(piece: PieceRef): Boolean =
    pieceBitset.contains(piece.pieceId.toInt)
  
  final def | (that: PieceSet): PieceSet =
    PieceSet.from(pieceBitset | that.pieceBitset)
  
  final def & (that: PieceSet): PieceSet =
    PieceSet.from(pieceBitset & that.pieceBitset)
  
  final def \ (that: PieceSet): PieceSet =
    PieceSet.from(pieceBitset -- that.pieceBitset)
  
  final def ^ (that: PieceSet): PieceSet =
    PieceSet.from(pieceBitset ^ that.pieceBitset)

object PieceSet:
  
  def empty: PieceSet = PieceSet()
  
  def apply(pieces: PieceRef*): PieceSet =
    PieceSet.Of(BitSet(pieces.map(_.pieceId.toInt)*))
    
  def from(pieceIds: BitSet): PieceSet =
    PieceSet.Of(pieceIds)
  
  private[pieces] case class Of (
    pieceBitset: BitSet = BitSet.empty,
  ) extends PieceSet