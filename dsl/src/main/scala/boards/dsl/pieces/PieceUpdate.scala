package boards.dsl.pieces

import boards.dsl.meta.PlayerId.PlayerId
import boards.dsl.pieces.PieceRef.PieceId
import boards.dsl.pieces.PieceUpdate
import boards.imports.games.{*, given}
import boards.imports.math.{*, given}
import boards.math.region.Region.HasRegionI
import boards.math.region.Vec.HasVecI
import boards.math.region.Ray
import boards.util.extensions.CollectionOps.contramap

sealed trait PieceUpdate extends PieceRef, HasRegionI:
  val time: Version

object PieceUpdate:
  
  def create(pieceId: PieceId, pieceType: PieceType, position: HasVecI, owner: PlayerId, time: Version): Create =
    Create(pieceId, pieceType, position.position, owner, time)
    
  def move(pieceId: PieceId, from: HasVecI, to: HasVecI, time: Version): Move =
    Move(pieceId, from.position, to.position, time)
    
  def destroy(pieceId: PieceId, position: HasVecI, time: Version): Destroy =
    Destroy(pieceId, position.position, time)
  
  case class Create (
    protected[pieces] val pieceId: PieceId,
    pieceType: PieceType,
    position: VecI,
    owner: PlayerId,
    override val time: Version,
  ) extends PieceUpdate:
    val region: RegionI = position.region
    override def toString = s"$pieceType -> $position"
  
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
    
  case class Destroy (
    protected[pieces] val pieceId: PieceId,
    position: VecI,
    override val time: Version,
  ) extends PieceUpdate:
    val region: RegionI = position.region
    override def toString = s"$position -> ∅"
  
  given Ordering[PieceUpdate] = Ordering.Int.contramap(_.time.toInt)
  
  trait UpdateQuery:
    
    val updates: Seq[PieceUpdate]
    
    lazy val creates: LazyList[Create] = LazyList.from(updates).collect:
      case create: Create => create
    
    lazy val moves: LazyList[Move] = LazyList.from(updates).collect:
      case move: Move => move
      
    def movesTo(region: HasRegionI): LazyList[Move] =
      moves.filter(_.to in region)
      
    def movesFrom(region: HasRegionI): LazyList[Move] =
      moves.filter(_.from in region)
    
    lazy val destroys: LazyList[Destroy] = LazyList.from(updates).collect:
      case destroy: Destroy => destroy
      
    def updated: PieceSet = PieceSet(updates*)
    def created: PieceSet = PieceSet(creates*)
    def moved: PieceSet = PieceSet(moves*)
    def movedTo(region: HasRegionI): PieceSet =
      PieceSet(movesTo(region)*)
    def movedFrom(region: HasRegionI): PieceSet =
      PieceSet(movesFrom(region)*)
    def destroyed: PieceSet = PieceSet(destroys*)
    
    def hasChanged: Boolean = updates.nonEmpty
    def hasCreated: Boolean = creates.nonEmpty
    def hasMoved: Boolean = moves.nonEmpty
    def hasMovedTo(region: HasRegionI): Boolean =
      movesTo(region).nonEmpty
    def hasMovedFrom(region: HasRegionI): Boolean =
      movesFrom(region).nonEmpty
    def hasDestroyed: Boolean = destroys.nonEmpty
    
    def updatedRegion: RegionI =
      updates.foldLeft(Region.empty[Int])(_ | _)
    
  object UpdateQuery:
    
    def of(updates: Seq[PieceUpdate]): UpdateQuery =
      UpdateQuery.Of(updates)
    
    private[pieces] case class Of (
      updates: Seq[PieceUpdate]
    ) extends UpdateQuery