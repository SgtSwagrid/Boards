package boards.dsl.pieces

import boards.dsl.pieces.PieceRef.PieceId
import boards.dsl.rules.Cause
import boards.dsl.states.HistoryState
import boards.dsl.states.HistoryState.AtTime

import scala.collection.immutable.BitSet

trait PieceRef extends PieceSet:
  
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
  
  opaque type PieceId = Int
  
  extension (pieceId: PieceId)
    def toInt: Int = pieceId
    def next: PieceId = pieceId + 1
    def toRef: PieceRef = PieceRef(toInt)
    
  object PieceId:
    def apply(pieceId: Int): PieceId = pieceId
    val initial = PieceId(0)