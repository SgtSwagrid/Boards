package boards.dsl.pieces

import boards.dsl.rules.{Cause, Control, Effect, Rule}
import boards.dsl.states.HistoryState.{AtTime, PeriodQuery}
import boards.dsl.pieces.PieceState
import boards.dsl.states.GameState
import boards.imports.games.{*, given}
import boards.imports.math.{*, given}
import boards.math.region.Region.HasRegionI
import boards.math.region.Vec.HasVecI
import boards.dsl.pieces.PieceRef.PieceId
import boards.dsl.pieces.PieceUpdate.UpdateQuery

import java.util.Random
import scala.annotation.targetName
import scala.collection.immutable.BitSet
import scala.reflect.ClassTag

/**
 * A game piece that exists somewhere on the game board.
 *
 * @param pieceType A flag indicating the type of piece this is.
 * @param position The current location of the piece on the game board.
 * @param owner The player ID (turn position) of the player who owns this piece.
 * @param pieceId An ID which uniquely identifies this piece.
 *           The ID remains the same even after the piece is moved.
 * @param lastUpdated The turn on which this piece was last modified.
 * @param previous The version of this piece prior to the most recent modification.
 */
case class Piece private[pieces] (
  pieceId: PieceId,
  pieceType: PieceType,
  position: VecI,
  owner: PlayerId,
  update: PieceUpdate,
  previous: Option[Piece],
) extends PieceRef, UpdateQuery, HasVecI:
  
  export position.{+, -, unary_-, `*`, in, directionTo, stepsTo, rayTo, rayUntil, ray, midpoint, x, y, z}
  
  val lastUpdated: Version = update.time
  
  def actions(using state: HistoryState): Rule =
    Rule(pieceType.rule(using state, this))
  
  def ownedBy(player: PlayerId): Boolean = owner == player
  def byOwner[X](x: X*): X = x(owner.toInt)
  infix def is(pieceType: Any): Boolean = this.pieceType == pieceType
  def is[Q](using C: ClassTag[Q]): Boolean =
    pieceType.getClass == C.runtimeClass
  
  val updates: LazyList[PieceUpdate] =
    update #:: previous.map(_.updates).getOrElse(LazyList.empty)
  
  val history: LazyList[Piece] =
    this #:: previous.map(_.history).getOrElse(LazyList.empty)
    
  def texture: Texture = pieceType.texture(this)
  
  /** The set of all current pieces with the same owner as this one. */
  def fellowPieces(using pieces: PieceState): PieceView = pieces.ofPlayer(owner)
  
  override def toString = s"$pieceType$position"