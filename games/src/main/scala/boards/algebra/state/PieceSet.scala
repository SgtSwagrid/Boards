package boards.algebra.state

import boards.algebra.rules.Rule
import boards.imports.games.{*, given}
import boards.imports.math.{*, given}
import boards.math.Region
import boards.math.Region.Shape

import scala.collection.immutable.BitSet
import scala.reflect.ClassTag

case class PieceSet (
  piecesByPos: Map[VecI, Piece] = Map.empty,
  selected: BitSet = BitSet.empty,
  
  playerFilter: Map[Game.PlayerId, BitSet] = Map.empty,
  typeFilter: Map[Class[? <: Piece.PieceType], BitSet] = Map.empty
) (using board: RegionI) extends Shape[Int]:
  
  def pieces: Iterable[Piece] = positions.flatMap(piecesByPos.get).toSeq
  def actions: Rule = Rule.union(pieces.map(_.actions))
  
  export selected.{isEmpty, nonEmpty}
  def contains(pos: VecI): Boolean =
    board.contains(pos) && board.indexOf.get(pos).exists(selected.contains)
  def get(pos: VecI): Option[Piece] =
    if contains(pos) then Some(piecesByPos(pos)) else None
    
  def forall(f: Piece => Boolean): Boolean =
    piecesByPos.values.forall(f)
    
  def exists(f: Piece => Boolean): Boolean =
    piecesByPos.values.exists(f)
    
  def belongsTo(pos: VecI, players: PlayerId*): Boolean =
    get(pos).exists(piece => players.contains(piece.owner))
  def isFriendly(pos: VecI)(using owner: PlayerId): Boolean =
    belongsTo(pos, owner)
  def isEnemy(pos: VecI)(using owner: PlayerId): Boolean =
    belongsTo(pos, (playerFilter.keySet - owner).toSeq*)
  
  def isType[P <: PieceType](pos: VecI)(using C: ClassTag[P]): Boolean =
    get(pos).exists(_.getClass == C.runtimeClass)
  
  def ofPlayer(owners: PlayerId*): PieceSet = copy (
    selected = owners
      .flatMap(playerFilter.get)
      .foldLeft(BitSet.empty)(_ | _) & selected
  )
  
  def ofFriendlyPlayer(using owner: PlayerId): PieceSet =
    ofPlayer(owner)
  def ofEnemyPlayers(using owner: PlayerId): PieceSet =
    ofPlayer((playerFilter.keySet - owner).toSeq*)
  
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
  
  def ofRegion(region: RegionI*): PieceSet = copy (
    selected = selected & BitSet(region.flatMap(_.positions)
      .filter(board.contains)
      .flatMap(board.indexOf.get)
    *)
  )
  
  def regionIsEmpty(region: RegionI*): Boolean = ofRegion(region*).isEmpty
  def regionNonEmpty(region: RegionI*): Boolean = ofRegion(region*).nonEmpty
  
  def filter(f: Piece => Boolean): PieceSet = copy (
    selected = selected.filter(id => board.posById.isDefinedAt(id) && get(board.posById(id)).exists(f))
  )
  
  def map[X](f: Piece => X): Iterable[X] =
    pieces.map(f)
    
  def move(f: VecI ?=> RegionI): Rule =
    Rule.union(pieces.map(_.move(f)).toSeq*)
  def replace(pieceTypes: PieceType*): Rule =
    Rule.union(pieces.map(_.replace(pieceTypes*)).toSeq*)
  def destroy: Rule =
    Generator.destroy(region)
    
  def relocate(to: RegionI): Rule =
    Effect.relocate(region -> to)
  def relocate(f: VecI ?=> VecI): Rule =
    Effect.relocate(region -> region.map(pos => f(using pos)))
  def substitute(pieceType: PieceType): Rule =
    Rule.sequence(pieces.map(_.substitute(pieceType)).toSeq*)
  def remove: Rule =
    Effect.remove(region)
  
  def withPiece(piece: Piece): PieceSet =
    withoutPiece(piece.position).copy (
      piecesByPos = piecesByPos + (piece.position -> piece),
      selected = selected ++ board.indexOf.get(piece),
      playerFilter = playerFilter + (piece.owner ->
        (playerFilter.getOrElse(piece.owner, BitSet.empty) ++ board.indexOf.get(piece))),
      typeFilter = typeFilter + (piece.pieceType.getClass ->
        (typeFilter.getOrElse(piece.pieceType.getClass, BitSet.empty) ++ board.indexOf.get(piece)))
    )
    
  def withMove(from: VecI, to: VecI): PieceSet =
    if contains(from) && board.contains(to) then
      val piece = get(from).get
      withoutPiece(from).withoutPiece(to).withPiece(piece.copy(position=to, hasMoved=true))
    else this
  
  def withoutPiece(pos: VecI): PieceSet =
    get(pos) match
      case None => this
      case Some(piece) => copy (
        piecesByPos = piecesByPos - piece.position,
        selected = selected -- board.indexOf.get(piece),
        playerFilter = playerFilter + (piece.owner ->
          (playerFilter(piece.owner) -- board.indexOf.get(piece))),
        typeFilter = typeFilter + (piece.pieceType.getClass ->
          (typeFilter(piece.pieceType.getClass) -- board.indexOf.get(piece)))
      )
      
  // Methods required to implement Shape:
  private lazy val region: RegionI = Region(selected.unsorted.flatMap: id =>
    Option.when(board.posById.isDefinedAt(id))(board.posById(id)))
  override def positions: Iterator[VecI] = region.positions
  override def start: VecI = board.start
  override def end: VecI = board.end
      
object PieceSet:
  def empty(using RegionI): PieceSet = PieceSet()