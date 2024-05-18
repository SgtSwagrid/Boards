package boards.algebra

import boards.algebra.Piece.PieceType
import boards.algebra.BoardState.given
import util.math.Pos.Pos
import util.structures.UniqueId

import scala.collection.immutable.BitSet
import PieceSet.*
import util.math.kernel.Kernel
import util.math.kernel.Kernel.given

import scala.reflect.ClassTag

case class PieceSet (
  piecesByPos: Map[Pos, Piece] = Map.empty,
  selected: BitSet = BitSet.empty,
  
  playerFilter: Map[Int, BitSet] = Map.empty,
  typeFilter: Map[Class[? <: PieceType], BitSet] = Map.empty
)(using board: Kernel[?]):
  
  def pieces: Set[Piece] = selected.unsorted.flatMap(piecesByPos.get)
  def positions: Kernel[?] = Kernel(pieces.map(_.position))
  def actions: Rule = pieces.map(_.actions)
  
  export selected.{isEmpty, nonEmpty}
  def contains(pos: Pos): Boolean =
    board.contains(pos) && selected.contains(pos)
  def get(pos: Pos): Option[Piece] =
    if contains(pos) then Some(piecesByPos(pos)) else None
    
  def belongsTo(pos: Pos, players: Int*): Boolean =
    get(pos).exists(piece => players.contains(piece.owner))
  def isFriendly(pos: Pos)(using state: BoardState): Boolean =
    belongsTo(pos, state.activePlayer)
  def isEnemy(pos: Pos)(using state: BoardState): Boolean =
    belongsTo(pos, state.inactivePlayers*)
  
  def isType[P <: PieceType](pos: Pos)(using C: ClassTag[P]): Boolean =
    get(pos).exists(_.getClass == C.runtimeClass)
  
  def ofPlayer(owners: Int*): PieceSet = copy (
    selected = owners
      .flatMap(playerFilter.get)
      .foldLeft(BitSet.empty)(_ | _) & selected
  )
  
  def ofActivePlayer(using state: BoardState): PieceSet =
    ofPlayer(state.activePlayer)
  def ofInactivePlayers(using state: BoardState): PieceSet =
    ofPlayer(state.inactivePlayers*)
  def ofNextPlayer(using state: BoardState): PieceSet =
    ofPlayer(state.nextPlayer)
  
  def ofType(pieceTypes: PieceType*): PieceSet = copy (
    selected = pieceTypes
      .flatMap(piece => typeFilter.get(piece.getClass))
      .foldLeft(BitSet.empty)(_ | _) & selected
  )
  
  def ofType[P <: PieceType](using C: ClassTag[P]): PieceSet = copy (
    selected = typeFilter.getOrElse(
      C.runtimeClass.asInstanceOf[Class[? <: PieceType]],
      BitSet.empty
    ) & selected
  )
  
  def ofRegion(region: Kernel[?]*): PieceSet = copy (
    selected = selected & BitSet(region.flatMap(_.positions)
      .filter(board.contains)
      .map(board.indexOf)
    *)
  )
  
  def regionIsEmpty(region: Kernel[?]*): Boolean = ofRegion(region*).isEmpty
  def regionNonEmpty(region: Kernel[?]*): Boolean = ofRegion(region*).nonEmpty
  
  def filter(f: Piece => Boolean): PieceSet = copy (
    selected = selected.filter(get(_).exists(f))
  )
  
  def map[X](f: Piece => X): Set[X] =
    pieces.map(f)
    
  def place
    (owner: Int)
    (placements: (Kernel[?], PieceType | Iterable[PieceType])*)
    (using board: Kernel[?])
  : PieceSet =
    
    val pieces = for
      (kernel, pieces) <- placements
      pieceSeq = pieces match
        case p: PieceType => Seq(p)
        case p: Iterable[?] => p.toSeq.asInstanceOf[Seq[PieceType]]
      (pos, i) <- kernel.positions.zipWithIndex
      if board.contains(pos)
    yield Piece(pieceSeq(i % pieceSeq.size), pos, owner)
    
    pieces.foldLeft(this)(_.addPiece(_))
    
  def place
    (placements: (Kernel[?], PieceType | Iterable[PieceType])*)
    (using state: BoardState)
  : PieceSet =
    place(state.activePlayer)(placements*)
    
  def move
    (moves: (Kernel[?], Kernel[?])*)
    (using board: Kernel[?])
  : PieceSet =
    
    val updates = for
      (regionFrom, regionTo) <- moves
      (from, to) <- regionFrom.positions.zip(regionTo.positions)
      if board.contains(from) && board.contains(to)
      piece <- get(from)
    yield piece -> to
    
    updates.foldLeft(this):
      case (set, piece -> to) => set.movePiece(piece, to)
      
  def destroy(positions: Kernel[?]*): PieceSet =
    positions.flatMap(_.positions).foldLeft(this)(_.removePiece(_))
  
  private def addPiece(piece: Piece): PieceSet =
    removePiece(piece.position).copy (
      piecesByPos = piecesByPos + (piece.position -> piece),
      selected = selected + piece.position,
      playerFilter = playerFilter + (piece.owner -> (playerFilter.getOrElse(piece.owner, BitSet.empty) + piece.position)),
      typeFilter = typeFilter + (piece.state.getClass -> (typeFilter.getOrElse(piece.state.getClass, BitSet.empty) + piece.position))
    )
    
  private def movePiece(from: Pos, to: Pos): PieceSet =
    if contains(from) && board.contains(to) then
      val piece = get(from).get
      removePiece(from).addPiece(piece.copy(position=to, hasMoved=true))
    else this
  
  private def removePiece(pos: Pos): PieceSet =
    get(pos) match
      case None => this
      case Some(piece) => copy (
        piecesByPos = piecesByPos - piece.position,
        selected = selected - piece.position,
        playerFilter = playerFilter + (piece.owner -> (playerFilter.getOrElse(piece.owner, BitSet.empty) - piece.position)),
        typeFilter = typeFilter + (piece.state.getClass -> (typeFilter.getOrElse(piece.state.getClass, BitSet.empty) - piece.position))
      )
      
object PieceSet:
  
  def empty(using Kernel[?]): PieceSet = PieceSet()
  
  given Conversion[PieceSet, Kernel[?]] with
    def apply(pieces: PieceSet): Kernel[?] = pieces.positions
    
  given Conversion[PieceSet, Rule] with
    def apply(pieces: PieceSet): Rule = pieces.actions
    
  given (using state: GameState): Conversion[PieceSet, Capability] with
    def apply(pieces: PieceSet): Capability =
      pieces.actions.from(state)