package boards.algebra

import boards.GameImports.{*, given}
import util.math.kernel.{Kernel, Ray}

trait Game:
  
  protected type GameBoard = Kernel[Colour]
  
  def setup: BoardState
  def rules: Rule
  def initial: InitialState = GameState.initial(setup, rules)
  
object Game:
  
  object State
  given (using state: GameState): Conversion[State.type, GameState] with
    def apply(s: State.type): GameState = state
  
  given (using state: GameState): Conversion[State.type, BoardState] with
    def apply(s: State.type): BoardState = state
  
  object Pieces
  given (using state: GameState): Conversion[Pieces.type, PieceSet] with
    def apply(p: Pieces.type): PieceSet = state.pieces
  
  extension (ray: Ray)
    
    def toPiece(using state: BoardState): Ray =
      ray.takeTo(state.contains)
    
    def toEmpty(using state: BoardState): Ray =
      ray.takeTo(!state.contains)
    
    def toFriendly(using state: BoardState): Ray =
      ray.takeTo(state.isFriendly)
    
    def toEnemy(using state: BoardState): Ray =
      ray.takeTo(state.isEnemy)
    
    def untilPiece(using state: BoardState): Ray =
      ray.takeUntil(state.contains)
    
    def untilEmpty(using state: BoardState): Ray =
      ray.takeTo(!state.contains)
    
    def untilFriendly(using state: BoardState): Ray =
      ray.takeUntil(state.isFriendly)
    
    def untilEnemy(using state: BoardState): Ray =
      ray.takeUntil(state.isEnemy)
  
  extension [T](kernel: Kernel[T])
    
    def ontoPiece(using state: BoardState): Kernel[T] =
      kernel.erode(!state.contains)
    
    def ontoEmpty(using state: BoardState): Kernel[T] =
      kernel.erode(state.contains)
    
    def ontoFriendly(using state: BoardState): Kernel[T] =
      kernel.erode(state.isFriendly)
    
    def ontoEnemy(using state: BoardState): Kernel[T] =
      kernel.erode(state.isEnemy)
      
    def avoidFriendly(using state: BoardState): Kernel[T] =
      kernel.erode(!state.isFriendly)
    
    def avoidEnemy(using state: BoardState): Kernel[T] =
      kernel.erode(!state.isEnemy)
  
  def byPlayer[X](x: X*)(using state: BoardState): X =
    x(state.activePlayer)