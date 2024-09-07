package boards.algebra

import boards.GameImports.{*, given}
import boards.algebra.Game.ID
import util.math.kernel.{Kernel, Ray}

trait Game:
  
  protected type GameBoard = Kernel[Colour]
  protected val Board: GameBoard
  
  def setup(numPlayers: Int): InstantaneousState ?=> InstantaneousState
  
  def rules: Rule
  def initial(numPlayers: Int): InitialState =
    val genesis = InstantaneousState.initial(Board, numPlayers)
    GameState.initial(setup(numPlayers)(using genesis), rules)
  
  val id = { ID += 1; ID - 1 }
  
  def numPlayers: Seq[Int] = Seq(2)
  
object Game:
  
  private var ID = 0
  
  object State
  given (using state: GameState): Conversion[State.type, GameState] with
    def apply(s: State.type): GameState = state
  given (using state: GameState): Conversion[State.type, InstantaneousState] with
    def apply(s: State.type): InstantaneousState = state.now
  
  object Pieces
  given (using state: InstantaneousState): Conversion[Pieces.type, PieceSet] with
    def apply(p: Pieces.type): PieceSet = state.pieces
    
  def hypothetically[T](state: GameState)(f: GameState ?=> T): T =
    f(using state)
  
  extension (ray: Ray)
    
    def toPiece(using InstantaneousState): Ray =
      ray.takeTo(Pieces.contains)
    
    def toEmpty(using InstantaneousState): Ray =
      ray.takeTo(!Pieces.contains)
    
    def toFriendly(using InstantaneousState): Ray =
      ray.takeTo(Pieces.isFriendly)
    
    def toEnemy(using InstantaneousState): Ray =
      ray.takeTo(Pieces.isEnemy)
    
    def untilPiece(using InstantaneousState): Ray =
      ray.takeUntil(Pieces.contains)
    
    def untilEmpty(using InstantaneousState): Ray =
      ray.takeTo(!Pieces.contains)
    
    def untilFriendly(using InstantaneousState): Ray =
      ray.takeUntil(Pieces.isFriendly)
    
    def untilEnemy(using InstantaneousState): Ray =
      ray.takeUntil(Pieces.isEnemy)
  
  extension [T](kernel: Kernel[T])
    
    def ontoPiece(using InstantaneousState): Kernel[T] =
      kernel.erode(!Pieces.contains)
    
    def ontoEmpty(using InstantaneousState): Kernel[T] =
      kernel.erode(Pieces.contains)
    
    def ontoFriendly(using InstantaneousState): Kernel[T] =
      kernel.erode(Pieces.isFriendly)
    
    def ontoEnemy(using InstantaneousState): Kernel[T] =
      kernel.erode(Pieces.isEnemy)
      
    def avoidFriendly(using InstantaneousState): Kernel[T] =
      kernel.erode(!Pieces.isFriendly)
    
    def avoidEnemy(using InstantaneousState): Kernel[T] =
      kernel.erode(!Pieces.isEnemy)
  
  def byPlayer[X](x: X*)(using state: InstantaneousState): X =
    x(state.activePlayer)