package boards.dsl.pieces

import boards.dsl.meta.PlayerRef.{PlayerId, PlayerRef}
import boards.dsl.pieces.PieceRef.PieceId
import boards.dsl.pieces.PieceState.Version
import boards.dsl.pieces.PieceUpdate
import boards.dsl.states.HistoryState
import boards.math.vector.Region.{HasRegion, RegionI}
import boards.math.vector.Vec.{HasVecI, VecI}
import boards.math.vector.{Ray, Region}
import boards.util.extensions.CollectionOps.contramap

import scala.collection.View

/** A [[PieceUpdate]] is a prior change to a [[Piece]].
  * Describes the exact manner in which and time at which a [[Piece]] was created, moved, or destroyed.
  * However, no information is provided as to what caused the modification.
  *
  * The [[PieceState]] stores all past [[PieceUpdate]]s and these are considered part of the state,
  * and can be queried to shape future control flow.
  */
sealed trait PieceUpdate extends PieceRef, HasRegion[Int]:
  val time: Version

object PieceUpdate:
  
  def create (pieceId: PieceId, pieceType: PieceType, position: VecI, owner: PlayerRef, time: Version): Create =
    Create(pieceId, pieceType, position.position, owner.playerId, time)
    
  def move (pieceId: PieceId, from: VecI, to: VecI, time: Version): Move =
    Move(pieceId, from.position, to.position, time)
    
  def destroy (pieceId: PieceId, position: VecI, time: Version): Destroy =
    Destroy(pieceId, position.position, time)
  
  /** A [[PieceUpdate]] describing the past creation of a new [[Piece]].
    * @param pieceId The [[PieceId]] of the new [[Piece]].
    * @param pieceType The original [[PieceType]] of the new [[Piece]].
    * @param position The original position of the new [[Piece]].
    * @param owner The original owner of the new [[Piece]].
    * @param time The time ([[Version]]) at which the new [[Piece]] was created.
    */
  case class Create (
    protected[pieces] val pieceId: PieceId,
    pieceType: PieceType,
    position: VecI,
    owner: PlayerId,
    override val time: Version,
  ) extends PieceUpdate, HasVecI:
    override def toString = s"$pieceType -> $position"
  
  /** A [[PieceUpdate]] describing the past movement of a [[Piece]].
    * @param pieceId The [[PieceId]] of the [[Piece]] that was moved.
    * @param from The original position of the [[Piece]].
    * @param to The new position of the [[Piece]].
    * @param time The time ([[Version]]) at which the [[Piece]] was moved.
    */
  case class Move (
    protected[pieces] val pieceId: PieceId,
    from: VecI,
    to: VecI,
    override val time: Version,
  ) extends PieceUpdate:
    val region: RegionI = from | to
    def step: VecI = to - from
    def direction: VecI = step.direction
    def midpoint: VecI = VecI.midpoint(from, to)
    def path: Ray = Ray.between(from, to)
    override def toString = s"$from -> $to"
  
  /** A [[PieceUpdate]] describing the past removal of a [[Piece]].
    * @param pieceId The [[PieceId]] of the [[Piece]] that was destroyed.
    * @param position The final position of the old [[Piece]].
    * @param time The time ([[Version]]) at which the [[Piece]] was destroyed.
    */
  case class Destroy (
    protected[pieces] val pieceId: PieceId,
    position: VecI,
    override val time: Version,
  ) extends PieceUpdate, HasVecI:
    override def toString = s"$position -> ∅"
  
  given Ordering[PieceUpdate] = Ordering.Int.contramap(_.time.toInt)
  
  /** Contains a collection of [[PieceUpdate]]s and methods for querying them. */
  trait UpdateQuery:
    
    /** Past modifications to the [[PieceState]], within some defined time window and in reverse chronological order. */
    val updates: Iterable[PieceUpdate]
    
    /** Past [[Piece]] creations, within some defined time window and in reverse chronological order. */
    lazy val creates: Iterable[Create] = updates.view.collect:
      case create: Create => create
    
    /** Past [[Piece]] movements, within some defined time window and in reverse chronological order. */
    lazy val moves: Iterable[Move] = updates.view.collect:
      case move: Move => move
    
    /** Past [[Piece]] movements into some [[RegionI]], within some defined time window and in reverse chronological order. */
    def movesTo (region: RegionI): Iterable[Move] =
      moves.filter(_.to in region)
    
    /** Past [[Piece]] movements out of some [[RegionI]], within some defined time window and in reverse chronological order. */
    def movesFrom (region: RegionI): Iterable[Move] =
      moves.filter(_.from in region)
    
    /** Past [[Piece]] removals, within some defined time window and in reverse chronological order. */
    lazy val destroys: Iterable[Destroy] = updates.view.collect:
      case destroy: Destroy => destroy
      
    /** All [[Piece]]s which were modified in some defined time window. */
    def updated: PieceSet = PieceSet(updates.toSeq*)
    
    /** All [[Piece]]s which were created in some defined time window. */
    def created: PieceSet = PieceSet(creates.toSeq*)
    
    /** All [[Piece]]s which were moved in some defined time window. */
    def moved: PieceSet = PieceSet(moves.toSeq*)
    
    /** All [[Piece]]s which were moved into some [[RegionI]] in some defined time window. */
    def movedTo (region: RegionI): PieceSet =
      PieceSet(movesTo(region).toSeq*)
    
    /** All [[Piece]]s which were moved out of some [[RegionI]] in some defined time window. */
    def movedFrom (region: RegionI): PieceSet =
      PieceSet(movesFrom(region).toSeq*)
      
    /** All [[Piece]]s which were destroyed in some defined time window. */
    def destroyed: PieceSet = PieceSet(destroys.toSeq*)
    
    /** Whether the [[PieceSet]] has changed in this time window. */
    def hasChanged: Boolean = updates.nonEmpty
    
    /** Whether any [[Piece]]s were created in this time window. */
    def hasCreated: Boolean = creates.nonEmpty
    
    /** Whether any [[Piece]]s were moved in this time window. */
    def hasMoved: Boolean = moves.nonEmpty
    
    /** Whether any [[Piece]]s were moved into some [[RegionI]] in this time window. */
    def hasMovedTo (region: RegionI): Boolean =
      movesTo(region).nonEmpty
    
    /** Whether any [[Piece]]s were moved out of some [[RegionI]] in this time window. */
    def hasMovedFrom (region: RegionI): Boolean =
      movesFrom(region).nonEmpty
      
    /** Whether any [[Piece]]s were destroyed in this time window. */
    def hasDestroyed: Boolean = destroys.nonEmpty
    
    /** All positions which were updated in the time window. */
    def updatedRegion: RegionI =
      updates.map(_.region).foldLeft(Region.empty[Int])(_ | _)
      
    /** The single most recent piece to have been created in this time window.
      * @throws NoSuchElementException if no pieces were created in this time window.
      */
    def latest (using HistoryState): Piece = creates.head.now.pieces.head
    
  object UpdateQuery:
    
    def of (updates: Iterable[PieceUpdate]): UpdateQuery =
      UpdateQuery.Of(updates)
    
    private[pieces] case class Of (
      updates: Iterable[PieceUpdate]
    ) extends UpdateQuery