package boards.algebra

import boards.algebra.rules.{Effect, Generator, Rule}
import boards.algebra.state.{GameState, Piece, PieceSet}
import boards.imports.games.{*, given}
import boards.imports.math.{*, given}
import boards.math.Ray

object shortcuts:
  
  export Rule.*
  export Generator.*
  export Effect.*
  
  def state(using state: GameState): GameState = state
  def pieces(using pieces: PieceSet): PieceSet = pieces
  
  def hypothetically[T](state: GameState)(f: GameState ?=> T): T =
    f(using state)
    
  def hypothetically[T](rule: Rule)(f: GameState ?=> T)(using state: GameState): T =
    f(using state.withRule(rule).flattenFutureSkips)
    
  def inCheck(pieceTypes: PieceType*)(using GameState): Boolean =
    pieces.ofInactivePlayers.canCaptureType(pieceTypes*)
  
  extension (ray: Ray)
    
    def toPiece(using PieceSet): Ray =
      ray.takeTo(pieces.contains)
    
    def toEmpty(using PieceSet): Ray =
      ray.takeTo(v => !pieces.contains(v))
    
    def toFriendly(using PieceSet, PlayerId): Ray =
      ray.takeTo(pieces.isFriendly)
    
    def toEnemy(using PieceSet, PlayerId): Ray =
      ray.takeTo(pieces.isEnemy)
    
    def untilPiece(using PieceSet): Ray =
      ray.takeUntil(pieces.contains)
    
    def untilEmpty(using PieceSet): Ray =
      ray.takeTo(v => !pieces.contains(v))
    
    def untilFriendly(using PieceSet, PlayerId): Ray =
      ray.takeUntil(pieces.isFriendly)
    
    def untilEnemy(using PieceSet, PlayerId): Ray =
      ray.takeUntil(pieces.isEnemy)
  
  extension (region: RegionI)
    
    def ontoPiece(using PieceSet): RegionI =
      region.filter(v => !pieces.contains(v))
    
    def ontoEmpty(using PieceSet): RegionI =
      region.filter(pieces.contains)
    
    def ontoFriendly(using PieceSet, PlayerId): RegionI =
      region.filter(pieces.isFriendly)
    
    def ontoEnemy(using PieceSet, PlayerId): RegionI =
      region.filter(pieces.isEnemy)
    
    def avoidFriendly(using PieceSet, PlayerId): RegionI =
      region.filter(!pieces.isFriendly)
    
    def avoidEnemy(using PieceSet, PlayerId): RegionI =
      region.filter(!pieces.isEnemy)
      
    def pieces(using pieces: PieceSet): PieceSet =
      pieces.ofRegion(region)
  
  def byPlayer[X](x: X*)(using player: PlayerId): X = x(player.toInt)
  
  given (using state: GameState): InstantaneousState = state.now
  given (using state: InstantaneousState): PlayerId = state.activePlayer
  given (using state: InstantaneousState): RegionI = state.board
  given (using state: InstantaneousState): PieceSet = state.pieces
  
  given Conversion[PieceSet, Rule] = _.actions
  given (using state: GameState): Conversion[PieceSet, Capability] = _.actions.from(state)
  
  given Conversion[Piece, Piece.PieceType] = _.pieceType
  given Conversion[Piece, Rule] = _.actions
  
  given Conversion[Iterable[Rule], Rule] = _.foldLeft(Rule.none)(_ | _)
  
  given Conversion[Action, Rule] = _.toRule