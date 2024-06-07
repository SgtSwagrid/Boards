package boards.algebra

import boards.GameImports.{*, given}
import util.math.kernel.{Kernel, Ray}

trait Game:
  
  protected type GameBoard = Kernel[Colour]
  protected val Board: GameBoard
  given GameBoard = Board
  
  given genesis: BoardState = BoardState(2)(using Board)
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
  given (using state: BoardState): Conversion[Pieces.type, PieceSet] with
    def apply(p: Pieces.type): PieceSet = state.pieces
    
  def hypothetically[T](state: GameState)(f: GameState ?=> T): T =
    f(using state)
  
  extension (ray: Ray)
    
    def toPiece(using BoardState): Ray =
      ray.takeTo(Pieces.contains)
    
    def toEmpty(using BoardState): Ray =
      ray.takeTo(!Pieces.contains)
    
    def toFriendly(using BoardState): Ray =
      ray.takeTo(Pieces.isFriendly)
    
    def toEnemy(using BoardState): Ray =
      ray.takeTo(Pieces.isEnemy)
    
    def untilPiece(using BoardState): Ray =
      ray.takeUntil(Pieces.contains)
    
    def untilEmpty(using BoardState): Ray =
      ray.takeTo(!Pieces.contains)
    
    def untilFriendly(using BoardState): Ray =
      ray.takeUntil(Pieces.isFriendly)
    
    def untilEnemy(using BoardState): Ray =
      ray.takeUntil(Pieces.isEnemy)
  
  extension [T](kernel: Kernel[T])
    
    def ontoPiece(using BoardState): Kernel[T] =
      kernel.erode(!Pieces.contains)
    
    def ontoEmpty(using BoardState): Kernel[T] =
      kernel.erode(Pieces.contains)
    
    def ontoFriendly(using BoardState): Kernel[T] =
      kernel.erode(Pieces.isFriendly)
    
    def ontoEnemy(using BoardState): Kernel[T] =
      kernel.erode(Pieces.isEnemy)
      
    def avoidFriendly(using BoardState): Kernel[T] =
      kernel.erode(!Pieces.isFriendly)
    
    def avoidEnemy(using BoardState): Kernel[T] =
      kernel.erode(!Pieces.isEnemy)
  
  def byPlayer[X](x: X*)(using state: BoardState): X =
    x(state.activePlayer)