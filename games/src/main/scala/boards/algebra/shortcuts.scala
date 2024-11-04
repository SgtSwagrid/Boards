package boards.algebra

import boards.algebra.rules.{Generator, Effect, Rule}
import boards.algebra.state.{GameState, Piece, PieceSet}
import boards.imports.games.{*, given}
import boards.imports.math.{*, given}

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
  
  extension (ray: Ray)
    
    def toPiece(using PieceSet): Ray =
      ray.takeTo(pieces.contains)
    
    def toEmpty(using PieceSet): Ray =
      ray.takeTo(!pieces.contains)
    
    def toFriendly(using InstantaneousState, PlayerId): Ray =
      ray.takeTo(pieces.isFriendly)
    
    def toEnemy(using InstantaneousState, PlayerId): Ray =
      ray.takeTo(pieces.isEnemy)
    
    def untilPiece(using PieceSet): Ray =
      ray.takeUntil(pieces.contains)
    
    def untilEmpty(using InstantaneousState): Ray =
      ray.takeTo(!pieces.contains)
    
    def untilFriendly(using InstantaneousState, PlayerId): Ray =
      ray.takeUntil(pieces.isFriendly)
    
    def untilEnemy(using InstantaneousState, PlayerId): Ray =
      ray.takeUntil(pieces.isEnemy)
  
  extension [T](kernel: Kernel[T])
    
    def ontoPiece(using PieceSet): Kernel[T] =
      kernel.erode(!pieces.contains)
    
    def ontoEmpty(using PieceSet): Kernel[T] =
      kernel.erode(pieces.contains)
    
    def ontoFriendly(using InstantaneousState, PlayerId): Kernel[T] =
      kernel.erode(pieces.isFriendly)
    
    def ontoEnemy(using InstantaneousState, PlayerId): Kernel[T] =
      kernel.erode(pieces.isEnemy)
    
    def avoidFriendly(using InstantaneousState, PlayerId): Kernel[T] =
      kernel.erode(!pieces.isFriendly)
    
    def avoidEnemy(using InstantaneousState, PlayerId): Kernel[T] =
      kernel.erode(!pieces.isEnemy)
      
    def pieces(using pieces: PieceSet): PieceSet =
      pieces.ofRegion(kernel)
  
  def byPlayer[X](x: X*)(using player: PlayerId): X = x(player.toInt)
  
  given (using state: GameState): InstantaneousState = state.now
  given (using state: InstantaneousState): Kernel[?] = state.board
  given (using state: InstantaneousState): PieceSet = state.pieces
  
  given (using state: InstantaneousState): PlayerId = state.activePlayer
  
  given Conversion[GameState, InstantaneousState] = _.now
  given (using state: GameState): Conversion[InstantaneousState, GameState] = state.withBoard
  
  given Conversion[InstantaneousState, PieceSet] = _.pieces
  //given (using state: InstantaneousState): Conversion[PieceSet, InstantaneousState] = state.withPieces
  //given (using state: GameState): Conversion[PieceSet, GameState] = state.withBoard
  
  given Conversion[PieceSet, Kernel[?]] = _.positions
  given Conversion[PieceSet, Rule] = _.actions
  given (using state: GameState): Conversion[PieceSet, Capability] = _.actions.from(state)
  
  given Conversion[Piece, VecI] = _.position
  given Conversion[Piece, Kernel.Shape] = p => Kernel(p.position)
  given Conversion[Piece, Piece.PieceType] = _.pieceType
  given Conversion[Piece, Rule] = _.actions
  
  given Conversion[Iterable[Rule], Rule] = _.foldLeft(Rule.none)(_ | _)
  
  given Conversion[Action, Rule] = _.toRule