package boards.dsl.pieces

import boards.dsl.pieces.PieceRef.PieceId
import boards.dsl.rules.Cause
import boards.dsl.states.HistoryState
import boards.dsl.states.HistoryState.AtTime

import scala.collection.immutable.BitSet

/** A [[PieceRef]] is a reference to a single [[Piece]], without any time-specific information.
  *
  * Use [[pieceRef.now]] or [[Pieces.get]] to query the current state of a [[PieceRef]].
  */
trait PieceRef extends PieceSet:
  
  /** The unique [[PieceId]] of the referenced [[Piece]]. */
  protected[pieces] val pieceId: PieceId
  protected[pieces] val pieceBitset: BitSet = BitSet(pieceId.toInt)
  
  override def equals(that: Any): Boolean =
    that match
      case that: PieceRef => this.pieceId == that.pieceId
      case _ => false

object PieceRef:
  
  def apply(pieceId: Int | PieceId): PieceRef = PieceHandle(pieceId)
  
  private[pieces] class PieceHandle (
    val pieceId: PieceId
  ) extends PieceRef
  
  /** The unique ID of a [[Piece]].
    * Assigned in increasing order and remains unchanged throughout the [[Piece]]'s existence,
    * in spite of possible modifications.
    */
  opaque type PieceId = Int
  
  extension (pieceId: PieceId)
    def toInt: Int = pieceId
    def next: PieceId = pieceId + 1
    def toRef: PieceRef = PieceRef(toInt)
    
  object PieceId:
    def apply(pieceId: Int): PieceId = pieceId
    val initial = PieceId(0)