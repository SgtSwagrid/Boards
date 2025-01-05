package boards.dsl

import boards.dsl.meta.PlayerRef.PlayerRef
import boards.dsl.pieces.{Piece, PieceType, PieceState, PieceRef, PieceView}
import boards.dsl.rules.{Cause, Effect, Rule}
import boards.dsl.states.{GameState, HistoryState, InstantaneousState}
import boards.imports.games.{*, given}
import boards.imports.math.{*, given}
import boards.math.region.Ray

object Shortcuts:
  
  def State(using state: HistoryState): HistoryState = state
  def Config(using HistoryState): GameConfig = State.config
  
  def hypothetically[T](state: HistoryState)(f: HistoryState ?=> T): T =
    f(using state)
    
  def hypothetically[T](effect: Effect)(f: HistoryState ?=> T)(using state: HistoryState): T =
    f(using effect.effect(state).value.history)
    
  def piece(using piece: Piece): Piece = piece
  
  extension (ray: Ray)
    
    def toPiece(using pieces: PieceState): Ray =
      ray.takeTo(pieces.contains)
    
    def toEmpty(using pieces: PieceState): Ray =
      ray.takeTo(v => !pieces.contains(v))
    
    def toFriendly(using pieces: PieceState, playerId: PlayerRef): Ray =
      ray.takeTo(pieces.isFriendly)
    
    def toEnemy(using pieces: PieceState, playerId: PlayerRef): Ray =
      ray.takeTo(pieces.isEnemy)
    
    def untilPiece(using pieces: PieceState): Ray =
      ray.takeUntil(pieces.contains)
    
    def untilEmpty(using pieces: PieceState): Ray =
      ray.takeTo(v => !pieces.contains(v))
    
    def untilFriendly(using pieces: PieceState, playerId: PlayerRef): Ray =
      ray.takeUntil(pieces.isFriendly)
    
    def untilEnemy(using pieces: PieceState, playerId: PlayerRef): Ray =
      ray.takeUntil(pieces.isEnemy)
      
    def whilePiece(using pieces: PieceState): Ray =
      ray.takeWhile(pieces.contains)
      
    def whileEmpty(using pieces: PieceState): Ray =
      ray.takeWhile(v => !pieces.contains(v))
      
    def whileFriendly(using pieces: PieceState, playerId: PlayerRef): Ray =
      ray.takeWhile(pieces.isFriendly)
      
    def whileEnemy(using pieces: PieceState, playerId: PlayerRef): Ray =
      ray.takeWhile(pieces.isEnemy)
  
  extension (region: RegionI)
    
    def rayFromPiece(using piece: PieceRef, state: HistoryState): Ray =
      region.rayFrom(piece.now.region)
    
    def ontoPiece(using pieces: PieceState): RegionI =
      region.filter(pieces.contains)
    
    def ontoEmpty(using pieces: PieceState): RegionI =
      region.filter(v => !pieces.contains(v))
    
    def ontoFriendly(using pieces: PieceState, playerId: PlayerRef): RegionI =
      region.filter(pieces.isFriendly)
    
    def ontoEnemy(using pieces: PieceState, playerId: PlayerRef): RegionI =
      region.filter(pieces.isEnemy)
    
    def avoidFriendly(using pieces: PieceState, playerId: PlayerRef): RegionI =
      region.filter(!pieces.isFriendly)
    
    def avoidEnemy(using pieces: PieceState, playerId: PlayerRef): RegionI =
      region.filter(!pieces.isEnemy)
      
    def pieces(using HistoryState): PieceView =
      Pieces.now.ofRegion(region)
      
    def clickAny: Cause = Cause.click(region)
    
    def clickRegion: Cause = Cause.click(region)
    
    def place (
      owner: HistoryState ?=> PlayerRef,
      pieceTypes: PieceType*,
    ): Rule =
      Control.place(owner, region, pieceTypes*)
      
    def placeMine (
      pieceTypes: PieceType*,
    ) (using PlayerRef): Rule =
      Control.placeMine(region, pieceTypes*)
      
    def fill (
      owner: HistoryState ?=> PlayerRef,
      pieceTypes: PieceType*,
    ): Rule =
      Control.fill(owner, region, pieceTypes*)
      
    def fillMine (
      pieceTypes: PieceType*,
    ) (using PlayerRef): Rule =
      Control.fillMine(region, pieceTypes*)
      
    def create (
      owner: HistoryState ?=> PlayerRef,
      pieceTypes: PieceType*,
    ): Effect =
      Effect.create(owner, region, pieceTypes*)
      
    def createMine (
      pieceTypes: PieceType*,
    ) (using PlayerRef): Effect =
      Effect.createMine(region, pieceTypes*)
    
  extension (position: VecI)
    
    def click(using HistoryState): Cause = Cause.click(position)
    
  extension (board: Board)
    def useAsBoard: Effect = Effect.setBoard(board)
  
  def byPlayer[X](x: X*)(using player: PlayerRef): X = x(player.playerId.toInt)
  
  given (using state: GameState): HistoryState = state.history
  given (using state: HistoryState): InstantaneousState = state.now
  given (using piece: Piece): PlayerRef = piece.owner
  given (using state: InstantaneousState): RegionI = state.board
  given (using state: InstantaneousState): PieceState = state.pieces
  given (using state: InstantaneousState): GameConfig = state.config