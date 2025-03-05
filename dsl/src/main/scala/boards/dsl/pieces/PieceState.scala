package boards.dsl.pieces

import boards.dsl.meta.PlayerRef.{PlayerId, PlayerRef}
import boards.dsl.pieces.PieceState.Version
import boards.dsl.pieces.{Piece, PieceRef, PieceType}
import boards.dsl.pieces.PieceRef.PieceId
import boards.math.vector.Region.RegionI
import boards.math.vector.Vec.{HasVecI, VecI}
import boards.math.vector.Region
import boards.util.extensions.CollectionOps.contramap

import scala.annotation.tailrec
import scala.reflect.ClassTag

/**
  * The complete state of all [[Piece]]s on the board, at a specific moment in time.
  * @param selected The set of all included [[PieceId]]s. Included for compatibility with [[PieceView]].
  * @param piecesById All existing [[Piece]]s, indexed by [[PieceId]].
  * @param piecesByPos All existing [[Piece]]s, indexed by `position`.
  * @param piecesByOwner All existing [[Piece]]s, indexed by `ownerId`.
  * @param piecesByType All existing [[Piece]]s, indexed by `pieceType`.
  * @param piecesByClass All existing [[Piece]]s, indexed by the class of `pieceType`.
  * @param updates All past modifications to the state, in reverse chronological order.
  * @param version The current version of the state, which uniquely identifies it.
  * @param board The region of the board on which pieces can exist.
  * @param nextPieceId The [[PieceId]] to be assigned to the next [[Piece]] created.
  *                    [[PieceId]]s are uniquely assigned in increasing order from `0`.
  *
  * @see [[PieceView]]
  *
  * @author Alec Dorrington
  */
case class PieceState (
  
  override val selected: PieceSet = PieceSet.empty,
  
  private[pieces] piecesById   : Map[PieceId,               Piece]    = Map.empty,
  private[pieces] piecesByPos  : Map[VecI,                  PieceId]  = Map.empty,
  private[pieces] piecesByOwner: Map[PlayerId,              PieceSet] = Map.empty,
  private[pieces] piecesByType : Map[PieceType,             PieceSet] = Map.empty,
  private[pieces] piecesByClass: Map[Class[? <: PieceType], PieceSet] = Map.empty,
  
  updates: List[PieceUpdate]    = List.empty,
  override val version: Version = Version.initial,
  
  board      : RegionI = Region.empty,
  private[pieces] nextPieceId: PieceId = PieceId.initial,
  
) extends PieceView:
  
  val pieces: LazyList[Piece] =
    pieceRefs.map(_.pieceId).map(piecesById.apply)
  
  def at(position: VecI): Option[Piece] =
    piecesByPos.get(position.position).map(piecesById)
  
  def get(pieceRef: PieceRef): Option[Piece] =
    piecesById.get(pieceRef.pieceId)
  
  def restrictTo(pieces: PieceSet): PieceView =
    PieceView.from(this, selected & pieces)
  
  /** Place a new [[Piece]] on the board.
    * If the position is out of bounds, this method does nothing.
    * If there is already a [[Piece]] at the position, that [[Piece]] is destroyed.
 *
    * @param pieceType The type of [[Piece]] to create.
    * @param position The position at which the [[Piece]] is to be placed.
    * @param owner The ID of the player who owns the [[Piece]].
    * @tparam P The type of [[Piece]] to create.
 *
    * @return A modified version of this [[PieceState]] with an extra [[Piece]] added.
    */
  @tailrec
  private[dsl] final def createPiece [P <: PieceType: ClassTag] (
    pieceType: P,
    position: VecI,
    owner: PlayerRef,
  ): PieceState =
    if !board.contains(position) then this
    else at(position) match
      case Some(captured) => destroyPiece(captured).createPiece(pieceType, position, owner)
      case None =>
        val update = PieceUpdate.create(nextPieceId, pieceType, position, owner, version)
        val piece = Piece (
          nextPieceId,
          pieceType,
          position.position,
          owner.playerId,
          update,
          None,
        )
        copy (
          selected = selected | piece,
          piecesById = piecesById + (piece.pieceId -> piece),
          piecesByPos = piecesByPos + (position.position -> piece.pieceId),
          piecesByOwner = piecesByOwner + (owner.playerId -> (piece | ofPlayer(owner))),
          piecesByType = piecesByType + (pieceType -> (piece | ofType(pieceType))),
          piecesByClass = piecesByClass + (pieceType.getClass -> (piece | ofClass(pieceType.getClass))),
          updates = update +: updates,
          version = version.next,
          nextPieceId = nextPieceId.next,
        )
  
  /** Move a [[Piece]] to a new position.
    * If the [[Piece]] no longer exists, or the new position is out of bounds, this method does nothing.
    * If the target position has another [[Piece]], that [[Piece]] is destroyed.
 *
    * @param piece The [[Piece]] to move somewhere else.
    * @param position The new position for the piece.
 *
    * @return A modified version of this [[PieceState]] with [[Piece]] moved to a new position.
    */
  @tailrec
  private[dsl] final def movePiece (
    piece: PieceRef,
    position: VecI,
  ): PieceState =
    piecesById.get(piece.pieceId) match
      case None => this
      case Some(original) =>
        if !board.contains(position) then this
        else at(position) match
          case Some(captured) => destroyPiece(captured).movePiece(piece, position)
          case None =>
            val update = PieceUpdate.move(original.pieceId, original.position, position, version)
            val piece = original.copy (
              position = position.position,
              update = update,
              previous = Some(original),
            )
            copy (
              piecesById = piecesById + (piece.pieceId -> piece),
              piecesByPos = piecesByPos - original.position + (position.position -> piece.pieceId),
              updates = update +: updates,
              version = version.next,
            )
  
  /** Remove a [[Piece]] from the board.
    * If the [[Piece]] no longer exists, this method does nothing.
    * @param piece The [[Piece]] to remove.
    * @return A modified version of this [[PieceState]] with [[Piece]] removed.
    */
  private[dsl] final def destroyPiece (
    piece: PieceRef,
  ): PieceState =
    piecesById.get(piece.pieceId) match
      case None => this
      case Some(piece @ Piece(id, pieceType, position, owner, previous, _)) => copy (
        selected = selected \ piece,
        piecesById = piecesById - id,
        piecesByPos = piecesByPos - position,
        piecesByOwner = piecesByOwner + (owner -> (ofPlayer(owner) \ piece)),
        piecesByType = piecesByType + (pieceType -> (ofType(pieceType) \ piece)),
        piecesByClass = piecesByClass + (pieceType.getClass -> (ofClass(pieceType.getClass) \ piece)),
        updates = PieceUpdate.destroy(piece.pieceId, position, version) +: updates,
        version = version.next,
      )

object PieceState:
  
  /** Create an empty [[PieceSet]] for an empty [[Board]]. */
  val empty: PieceState = PieceState()
  
  /** Create an empty [[PieceSet]] for the given [[Board]].
    * Each [[PieceSet]] stores the [[Board]] so that bounds checks can be applied.
    */
  def forBoard (board: RegionI): PieceState =
    PieceState(board = board)
  
  /** The version number of a [[PieceSet]], used for chronological comparison. */
  final class Version(val toInt: Int):
    
    def next: Version = Version(toInt + 1)
    val version: Version = this
    override def toString = s"$toInt"
  
  object Version:
    
    def initial: Version = Version(0)
    given Ordering[Version] = Ordering.Int.contramap(_.toInt)