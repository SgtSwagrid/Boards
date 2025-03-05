package boards.dsl.pieces

import boards.dsl.meta.PlayerRef.PlayerId
import boards.dsl.pieces.PieceRef.PieceId
import boards.dsl.pieces.PieceState.Version
import boards.dsl.pieces.PieceUpdate.UpdateQuery
import boards.dsl.rules.Rule
import boards.dsl.states.HistoryState
import boards.graphics.Texture
import boards.math.vector.Vec.{HasVecI, VecI}

import scala.reflect.ClassTag

/**
 * A game piece that exists somewhere on the game board at a particular time.
 *
 * @param pieceId An ID which uniquely identifies this piece.
  *               The ID remains the same even after the piece is moved or modified.
 * @param pieceType A flag indicating the type of piece this is.
 * @param position The current location of the piece on the game board.
 * @param owner The player ID (turn position) of the player who owns this piece.
 * @param update The most recent modification made to this piece.
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
  
  /** The time at which this piece was last modified. */
  val lastUpdated: Version = update.time
  
  def actions (using state: HistoryState): Rule =
    Rule(pieceType.rule(using state, this))
  
  /** Determine whether this piece is owned by the given player. */
  def ownedBy (player: PlayerId): Boolean = owner == player
  
  /** Select a value among those given based on the owner of this piece.
    * i.e. if this piece is owned by player 0, then `byOwner(a, b, c)` is equivalent to `a`.
    */
  def byOwner [X] (x: X*): X = x(owner.playerId.toInt)
  
  /** Determine whether this piece has the given [[PieceType]]. */
  infix def is (pieceType: Any): Boolean = this.pieceType == pieceType
  
  /** Determine whether this piece has a [[PieceType]] of the specified class. */
  def is [P: ClassTag as C]: Boolean =
    pieceType.getClass == C.runtimeClass
  
  /** The set of all previous modifications to this piece, in reverse chronological order. */
  val updates: LazyList[PieceUpdate] =
    update #:: previous.map(_.updates).getOrElse(LazyList.empty)
  
  /** The set of all previous versions of this piece, including the most current, in reverse chronological order. */
  val history: LazyList[Piece] =
    this #:: previous.map(_.history).getOrElse(LazyList.empty)
    
  /** The current texture of this piece. */
  def texture: Texture = pieceType.texture(this)
  
  /** The set of all current pieces with the same owner as this one. */
  def fellowPieces (using pieces: PieceState): PieceView = pieces.ofPlayer(owner)
  
  override def toString = s"$pieceType$position"