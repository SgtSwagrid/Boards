package boards.algebra

import boards.imports.games.{*, given}
import boards.imports.math.{*, given}

import scala.collection.immutable.BitSet
import scala.reflect.ClassTag

import boards.algebra.Shortcuts.given_Conversion_Iterable_Rule

case class PieceSet (
  piecesByPos: Map[VecI, Piece] = Map.empty,
  selected: BitSet = BitSet.empty,
  
  playerFilter: Map[Int, BitSet] = Map.empty,
  typeFilter: Map[Class[? <: boards.algebra.Piece.PieceType], BitSet] = Map.empty
)(using board: Kernel[?]):
  
  def pieces: Set[Piece] = selected.unsorted.flatMap(piecesByPos.get)
  def positions: Kernel[?] = Kernel(pieces.map(_.position))
  def actions: Rule = pieces.map(_.actions)
  
  export selected.{isEmpty, nonEmpty}
  def contains(pos: VecI): Boolean =
    board.contains(pos) && selected.contains(pos)
  def get(pos: VecI): Option[Piece] =
    if contains(pos) then Some(piecesByPos(pos)) else None
    
  def forall(f: Piece => Boolean): Boolean =
    piecesByPos.values.forall(f)
    
  def exists(f: Piece => Boolean): Boolean =
    piecesByPos.values.exists(f)
    
  def belongsTo(pos: VecI, players: Int*): Boolean =
    get(pos).exists(piece => players.contains(piece.owner))
  def isFriendly(pos: VecI)(using state: InstantaneousState): Boolean =
    belongsTo(pos, state.activePlayer)
  def isEnemy(pos: VecI)(using state: InstantaneousState): Boolean =
    belongsTo(pos, state.inactivePlayers*)
  
  def isType[P <: PieceType](pos: VecI)(using C: ClassTag[P]): Boolean =
    get(pos).exists(_.getClass == C.runtimeClass)
  
  def ofPlayer(owners: Int*): PieceSet = copy (
    selected = owners
      .flatMap(playerFilter.get)
      .foldLeft(BitSet.empty)(_ | _) & selected
  )
  
  def ofActivePlayer(using state: InstantaneousState): PieceSet =
    ofPlayer(state.activePlayer)
  def ofInactivePlayers(using state: InstantaneousState): PieceSet =
    ofPlayer(state.inactivePlayers*)
  def ofNextPlayer(using state: InstantaneousState): PieceSet =
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
    
  def insert
    (owner: Int)
    (placements: (PieceType | Iterable[PieceType], Kernel[?])*)
    (using board: Kernel[?])
  : PieceSet =
    
    val pieces = for
      (pieces, kernel) <- placements
      pieceSeq = pieces match
        case p: PieceType => Seq(p)
        case p: Iterable[?] => p.toSeq.asInstanceOf[Seq[PieceType]]
      (pos, i) <- kernel.positions.zipWithIndex
      if board.contains(pos)
    yield Piece(pieceSeq(i % pieceSeq.size), pos, owner)
    
    pieces.foldLeft(this)(_.addPiece(_))
    
  def insert
    (placements: (PieceType | Iterable[PieceType], Kernel[?])*)
    (using state: InstantaneousState)
  : PieceSet =
    insert(state.activePlayer)(placements*)
    
  def relocate
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
      case (set, piece -> to) => set.movePiece(piece.position, to)
      
  def remove(positions: Kernel[?]*): PieceSet =
    positions.flatMap(_.positions).foldLeft(this)(_.removePiece(_))
  
  private def addPiece(piece: Piece): PieceSet =
    removePiece(piece.position).copy (
      piecesByPos = piecesByPos + (piece.position -> piece),
      selected = selected + piece.position,
      playerFilter = playerFilter + (piece.owner -> (playerFilter.getOrElse(piece.owner, BitSet.empty) + piece.position)),
      typeFilter = typeFilter + (piece.pieceType.getClass -> (typeFilter.getOrElse(piece.pieceType.getClass, BitSet.empty) + piece.position))
    )
    
  private def movePiece(from: VecI, to: VecI): PieceSet =
    if contains(from) && board.contains(to) then
      val piece = get(from).get
      removePiece(from).addPiece(piece.copy(position=to, hasMoved=true))
    else this
  
  private def removePiece(pos: VecI): PieceSet =
    get(pos) match
      case None => this
      case Some(piece) => copy (
        piecesByPos = piecesByPos - piece.position,
        selected = selected - piece.position,
        playerFilter = playerFilter + (piece.owner -> (playerFilter.getOrElse(piece.owner, BitSet.empty) - piece.position)),
        typeFilter = typeFilter + (piece.pieceType.getClass -> (typeFilter.getOrElse(piece.pieceType.getClass, BitSet.empty) - piece.position))
      )
      
object PieceSet:
  def empty(using Kernel[?]): PieceSet = PieceSet()