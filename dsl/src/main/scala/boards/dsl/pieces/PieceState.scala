package boards.dsl.pieces

import boards.dsl.meta.PlayerRef.{PlayerId, PlayerRef}
import boards.dsl.pieces.PieceState.Version
import boards.dsl.pieces.{Piece, PieceRef, PieceType}
import boards.dsl.pieces.PieceRef.PieceId
import boards.math.region.Region.{HasRegionI, RegionI}
import boards.math.region.Vec.{HasVecI, VecI}
import boards.math.region.Region
import boards.util.extensions.CollectionOps.contramap

import scala.reflect.ClassTag

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
  
  def at(position: HasVecI): Option[Piece] =
    piecesByPos.get(position.position).map(piecesById)
    
  def get(pieceRef: PieceRef): Option[Piece] =
    piecesById.get(pieceRef.pieceId)
  
  def restrictTo(pieces: PieceSet): PieceView =
    PieceView.from(this, selected & pieces)
  
  def createPiece[P <: PieceType: ClassTag] (
    pieceType: P,
    position: HasVecI,
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
  
  def movePiece (
    piece: PieceRef,
    position: HasVecI,
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
  
  def destroyPiece (
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
  
  val empty: PieceState = PieceState()
  def forBoard(board: HasRegionI): PieceState =
    PieceState(board = board.region)
  
  final class Version(val toInt: Int):
    
    def next: Version = Version(toInt + 1)
    val version: Version = this
    override def toString = s"$toInt"
  
  object Version:
    
    def initial: Version = Version(0)
    given Ordering[Version] = Ordering.Int.contramap(_.toInt)