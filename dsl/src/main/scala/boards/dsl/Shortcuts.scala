package boards.dsl

import boards.dsl.meta.Game.{Board, GameConfig, Property}
import boards.dsl.meta.PlayerRef.PlayerRef
import boards.dsl.pieces.{Piece, PieceRef, PieceSet, PieceState, PieceType, PieceView}
import boards.dsl.rules.{Cause, Control, Effect, Rule}
import boards.dsl.states.{GameState, HistoryState, InstantaneousState}
import boards.math.algebra.Unbounded
import boards.math.algebra.Unbounded.Finite
import boards.math.vector.Ray
import boards.math.vector.Region.RegionI
import boards.math.vector.Vec.VecI
import boards.util.extensions.FunctionOps.unary_!
import boards.math.algebra.Algebra.Numeric

object Shortcuts:
  
  def hypothetically [T] (state: HistoryState) (f: (state: HistoryState) ?=> T): T =
    f(using state)
    
  def hypothetically [T] (effect: Effect) (f: (state: HistoryState) ?=> T) (using state: HistoryState): T =
    f(using effect.effect(state).value.history)
  
  extension (ray: Ray)
    
    def toPiece (using pieces: PieceState): Ray =
      ray.takeTo(pieces.contains)
    
    def toEmpty (using pieces: PieceState): Ray =
      ray.takeTo(v => !pieces.contains(v))
    
    def toFriendly (using pieces: PieceState, playerId: PlayerRef): Ray =
      ray.takeTo(pieces.isFriendly)
    
    def toEnemy (using pieces: PieceState, playerId: PlayerRef): Ray =
      ray.takeTo(pieces.isEnemy)
    
    def untilPiece (using pieces: PieceState): Ray =
      ray.takeUntil(pieces.contains)
    
    def untilEmpty (using pieces: PieceState): Ray =
      ray.takeTo(v => !pieces.contains(v))
    
    def untilFriendly (using pieces: PieceState, playerId: PlayerRef): Ray =
      ray.takeUntil(pieces.isFriendly)
    
    def untilEnemy (using pieces: PieceState, playerId: PlayerRef): Ray =
      ray.takeUntil(pieces.isEnemy)
      
    def whilePiece (using pieces: PieceState): Ray =
      ray.takeWhile(pieces.contains)
      
    def whileEmpty (using pieces: PieceState): Ray =
      ray.takeWhile(v => !pieces.contains(v))
      
    def whileFriendly (using pieces: PieceState, playerId: PlayerRef): Ray =
      ray.takeWhile(pieces.isFriendly)
      
    def whileEnemy (using pieces: PieceState, playerId: PlayerRef): Ray =
      ray.takeWhile(pieces.isEnemy)
      
    def whileInBounds (using state: HistoryState): Ray =
      ray.takeWhile(state.board.inLogicalBounds)
  
  extension (region: RegionI)
    
    def rayFromPiece (using piece: PieceRef, state: HistoryState): Ray =
      region.rayFrom(piece.now.region)
    
    def ontoPiece (using pieces: PieceState): RegionI =
      region.filter(pieces.contains)
    
    def ontoEmpty (using pieces: PieceState): RegionI =
      region.filter(v => !pieces.contains(v))
    
    def ontoFriendly (using pieces: PieceState, playerId: PlayerRef): RegionI =
      region.filter(pieces.isFriendly)
    
    def ontoEnemy (using pieces: PieceState, playerId: PlayerRef): RegionI =
      region.filter(pieces.isEnemy)
    
    def avoidFriendly (using pieces: PieceState, playerId: PlayerRef): RegionI =
      region.filter(!pieces.isFriendly)
    
    def avoidEnemy (using pieces: PieceState, playerId: PlayerRef): RegionI =
      region.filter (!pieces.isEnemy)
      
    def clickAny: Cause = Cause.click(region)
    
    def clickRegion: Cause = Cause.click(region)
    
    def place (
      owner: (state: HistoryState) ?=> PlayerRef,
      pieceTypes: PieceType*,
    ): Rule =
      Control.place(owner, region, pieceTypes*)
      
    def placeFriendly (
      pieceTypes: PieceType*,
    ) (using PlayerRef): Rule =
      Control.placeFriendly(region, pieceTypes*)
      
    def fill (
      owner: (state: HistoryState) ?=> PlayerRef,
      pieceTypes: PieceType*,
    ): Rule =
      Control.fill(owner, region, pieceTypes*)
      
    def fillFriendly (
      pieceTypes: PieceType*,
    ) (using PlayerRef): Rule =
      Control.fillFriendly(region, pieceTypes*)
      
    def create (
      owner: (state: HistoryState) ?=> PlayerRef,
      pieceTypes: PieceType*,
    ): Effect =
      Effect.create(owner, region, pieceTypes*)
      
    def createFriendly (
      pieceTypes: PieceType*,
    ) (using PlayerRef): Effect =
      Effect.createFriendly(region, pieceTypes*)
      
    def pieces (using state: HistoryState): PieceView =
      state.pieces.ofRegion(region)
    
  extension (position: VecI)
    
    def click (using HistoryState): Cause = Cause.click(position)
    
  extension (board: Board)
    
    def useAsBoard: Effect = Effect.setBoard(board)
  
  def byPlayer [X] (x: X*) (using player: PlayerRef): X = x(player.playerId.toInt)
  def byActivePlayer [X] (x: X*) (using state: HistoryState): X = x(state.activePlayer.toInt)
  
  given (using state: GameState): HistoryState = state.history
  given (using state: HistoryState): InstantaneousState = state.now
  given (using piece: Piece): PlayerRef = piece.owner
  given (using state: InstantaneousState): RegionI = state.board
  given (using state: InstantaneousState): PieceState = state.pieces
  given (using state: InstantaneousState): GameConfig = state.config
  
  given (using GameConfig): Conversion[Property, Int] = _.get
  
  given Conversion[Piece, VecI] = _.position
  given Conversion[PieceView, RegionI] = _.region
  given (using piece: Piece): VecI = piece.position
  
  given [X: Numeric]: Conversion[X, Unbounded[X]] = x => Finite(x)