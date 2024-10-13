package boards.algebra

import boards.imports.games.{*, given}
import boards.imports.math.{*, given}

object shortcuts:
  
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
    
    def toFriendly(using InstantaneousState, PlayerId): Ray =
      ray.takeTo(Pieces.isFriendly)
    
    def toEnemy(using InstantaneousState, PlayerId): Ray =
      ray.takeTo(Pieces.isEnemy)
    
    def untilPiece(using InstantaneousState): Ray =
      ray.takeUntil(Pieces.contains)
    
    def untilEmpty(using InstantaneousState): Ray =
      ray.takeTo(!Pieces.contains)
    
    def untilFriendly(using InstantaneousState, PlayerId): Ray =
      ray.takeUntil(Pieces.isFriendly)
    
    def untilEnemy(using InstantaneousState, PlayerId): Ray =
      ray.takeUntil(Pieces.isEnemy)
  
  extension [T](kernel: Kernel[T])
    
    def ontoPiece(using InstantaneousState): Kernel[T] =
      kernel.erode(!Pieces.contains)
    
    def ontoEmpty(using InstantaneousState): Kernel[T] =
      kernel.erode(Pieces.contains)
    
    def ontoFriendly(using InstantaneousState, PlayerId): Kernel[T] =
      kernel.erode(Pieces.isFriendly)
    
    def ontoEnemy(using InstantaneousState, PlayerId): Kernel[T] =
      kernel.erode(Pieces.isEnemy)
    
    def avoidFriendly(using InstantaneousState, PlayerId): Kernel[T] =
      kernel.erode(!Pieces.isFriendly)
    
    def avoidEnemy(using InstantaneousState, PlayerId): Kernel[T] =
      kernel.erode(!Pieces.isEnemy)
  
  def byPlayer[X](x: X*)(using player: PlayerId): X = x(player.toInt)
  
  given Conversion[GameState, InstantaneousState] with
    def apply(state: GameState): InstantaneousState = state.now
  
  given (using state: GameState): Conversion[InstantaneousState, GameState] with
    def apply(board: InstantaneousState): GameState = state.withBoard(board)
    
  given (using state: GameState): Conversion[PieceSet, GameState] with
    def apply(pieces: PieceSet): GameState = state.withBoard(state.now.withPieces(pieces))
  
  given (using state: GameState): InstantaneousState = state.now
  
  given (using state: InstantaneousState): Kernel[?] = state.board
  given (using state: InstantaneousState): PieceSet = state.pieces
  
  given Conversion[PieceSet, Kernel[?]] with
    def apply(pieces: PieceSet): Kernel[?] = pieces.positions
  
  given Conversion[PieceSet, Rule] with
    def apply(pieces: PieceSet): Rule = pieces.actions
  
  given (using state: InstantaneousState): Conversion[PieceSet, InstantaneousState] with
    def apply(pieces: PieceSet): InstantaneousState = state.withPieces(pieces)
  
  given (using state: GameState): Conversion[PieceSet, Capability] with
    def apply(pieces: PieceSet): Capability =
      pieces.actions.from(state)
  
  given Conversion[Piece, VecI] with
    def apply(piece: Piece): VecI = piece.position
  
  given Conversion[Piece, Kernel.Shape] with
    def apply(piece: Piece): Kernel.Shape = Kernel(piece.position)
  
  given Conversion[Piece, Piece.PieceType] with
    def apply(piece: Piece): Piece.PieceType = piece.pieceType
  
  given Conversion[Piece, Rule] with
    def apply(piece: Piece): Rule = piece.actions
  
  given Conversion[Iterable[Rule], Rule] with
    def apply(rules: Iterable[Rule]): Rule = rules.foldLeft(Rule.none)(_ | _)
  
  given [X <: Rule | InstantaneousState | Boolean | Outcome | PieceSet]: Conversion[X, PartialFunction[GameState, X]] with
    def apply(x: X): PartialFunction[GameState, X] = _ => x
  
  given Conversion[PieceSet, PartialFunction[GameState, InstantaneousState]] with
    def apply(pieces: PieceSet): PartialFunction[GameState, InstantaneousState] =
      gameState => gameState.now.withPieces(pieces)
  
  given Conversion[Action, Rule] with
    def apply(action: Action): Rule =
      action match
        case Place(owner, piece, pos) => Generator.place(owner)(piece -> pos)
        case Move(_, from, to) => Generator.move(from -> to)
        case Destroy(piece) => Generator.destroy(piece)
        case NoOp => Generator.skip