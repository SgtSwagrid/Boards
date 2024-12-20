package boards.dsl

import boards.dsl.pieces.{Piece, PieceState}
import boards.dsl.rules.{Cause, Effect, Rule}
import boards.dsl.states.{GameState, InstantaneousState}
import boards.imports.games.{*, given}
import boards.imports.math.{*, given}
import boards.math.region.Ray

object shortcuts:
  
  export Rule.*
  export Cause.*
  export Effect.*
  
  def State(using state: HistoryState): HistoryState = state
  
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
    
    def toFriendly(using pieces: PieceState, playerId: PlayerId): Ray =
      ray.takeTo(pieces.isFriendly)
    
    def toEnemy(using pieces: PieceState, playerId: PlayerId): Ray =
      ray.takeTo(pieces.isEnemy)
    
    def untilPiece(using pieces: PieceState): Ray =
      ray.takeUntil(pieces.contains)
    
    def untilEmpty(using pieces: PieceState): Ray =
      ray.takeTo(v => !pieces.contains(v))
    
    def untilFriendly(using pieces: PieceState, playerId: PlayerId): Ray =
      ray.takeUntil(pieces.isFriendly)
    
    def untilEnemy(using pieces: PieceState, playerId: PlayerId): Ray =
      ray.takeUntil(pieces.isEnemy)
  
  extension (region: RegionI)
    
    def ontoPiece(using pieces: PieceState): RegionI =
      region.filter(v => !pieces.contains(v))
    
    def ontoEmpty(using pieces: PieceState): RegionI =
      region.filter(pieces.contains)
    
    def ontoFriendly(using pieces: PieceState, playerId: PlayerId): RegionI =
      region.filter(pieces.isFriendly)
    
    def ontoEnemy(using pieces: PieceState, playerId: PlayerId): RegionI =
      region.filter(pieces.isEnemy)
    
    def avoidFriendly(using pieces: PieceState, playerId: PlayerId): RegionI =
      region.filter(!pieces.isFriendly)
    
    def avoidEnemy(using pieces: PieceState, playerId: PlayerId): RegionI =
      region.filter(!pieces.isEnemy)
      
    def pieces(using HistoryState): PieceView =
      Pieces.now.ofRegion(region)
      
    def clickAny: Cause = Cause.click(region)
    
    def clickRegion: Cause = Cause.click(region)
    
    def place (
      owner: HistoryState ?=> PlayerId,
      pieceTypes: PieceType*,
    ): Rule =
      Control.place(owner, region, pieceTypes*)
      
    def placeMine (
      pieceTypes: PieceType*,
    ) (using PlayerId): Rule =
      Control.placeMine(region, pieceTypes*)
      
    def fill (
      owner: HistoryState ?=> PlayerId,
      pieceTypes: PieceType*,
    ): Rule =
      Control.fill(owner, region, pieceTypes*)
      
    def fillMine (
      pieceTypes: PieceType*,
    ) (using PlayerId): Rule =
      Control.fillMine(region, pieceTypes*)
      
    def create (
      owner: HistoryState ?=> PlayerId,
      pieceTypes: PieceType*,
    ): Effect =
      Effect.create(owner, region, pieceTypes*)
      
    def createMine (
      pieceTypes: PieceType*,
    ) (using PlayerId): Effect =
      Effect.createMine(region, pieceTypes*)
    
  extension (position: VecI)
    
    def click(using HistoryState): Cause = Cause.click(position)
  
  def byPlayer[X](x: X*)(using playerId: PlayerId): X = x(playerId.toInt)
  
  given (using state: GameState): HistoryState = state.history
  given (using state: HistoryState): InstantaneousState = state.now
  given (using piece: Piece): PlayerId = piece.owner
  given (using state: InstantaneousState): RegionI = state.board
  given (using state: InstantaneousState): PieceState = state.pieces
  given (using state: InstantaneousState): GameConfig = state.config