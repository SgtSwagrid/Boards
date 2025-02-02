package boards.games; import boards.imports.all.{*, given}

object Chess extends Game:
  
  override val name = "Chess"
  
  val white = Player(0, "White", Colour.British.LynxWhite)
  val black = Player(1, "Black", Colour.British.MattPurple)
  override val players = Seq(white, black)
  
  override val board = Box(8, 8)
    .withLabels(Pattern.Checkered(Colour.Chess.Dark, Colour.Chess.Light))
  
  override def setup =
    board.row(7).create(black, Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook) |>
    board.row(6).create(black, Pawn) |>
    board.row(1).create(white, Pawn) |>
    board.row(0).create(white, Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook)
  
  override def loop = Rule.alternatingTurns:
    Pieces.ofActivePlayer.actions.require(!inCheck)
      .orElseStop(if inCheck then State.nextPlayer.wins else Draw)
  
  def inCheck(using state: HistoryState) = King.ofActivePlayer.inCheck
  
  object Rook extends
    TexturedPiece(Texture.WhiteRook, Texture.BlackRook),
    MoveablePiece(Dir.orthogonal.rayFromHere.toEnemy.untilFriendly)
  
  object Knight extends
    TexturedPiece(Texture.WhiteKnight, Texture.BlackKnight),
    MoveablePiece(Dir.knight(1, 2).fromHere.avoidFriendly)
  
  object Bishop extends
    TexturedPiece(Texture.WhiteBishop, Texture.BlackBishop),
    MoveablePiece(Dir.diagonal.rayFromHere.toEnemy.untilFriendly)
  
  object Queen extends
    TexturedPiece(Texture.WhiteQueen, Texture.BlackQueen),
    MoveablePiece(Dir.octagonal.rayFromHere.toEnemy.untilFriendly)
  
  object King extends TexturedPiece(Texture.WhiteKing, Texture.BlackKing):
    def rule = r_move | r_castle
    
    def r_move(using Piece) = Control.moveThis:
      Dir.octagonal.fromHere.avoidFriendly
      
    def r_castle(using king: Piece) = Rule.when(!king.hasMoved):
      Rule.union:
        king.fellowPieces.ofType(Rook).pieces
          .filter: rook =>
            val path = king.rayTo(rook)
            !rook.hasMoved &&
            rook.y == Piece.y &&
            path.interior.pieces.isEmpty &&
            !Pieces.ofInactivePlayers.following(King.createMine(path)).canMoveTo(path)
          .map: rook =>
            king.move(king + (king.directionTo(rook) * 2)) |>
            rook.relocate(king + king.directionTo(rook))
  
  object Pawn extends TexturedPiece(Texture.WhitePawn, Texture.BlackPawn):
    def rule = (r_move | r_capture | r_enpassant) |> r_promote
    
    def forward (using Piece) = Piece.byOwner(Dir.up, Dir.down)
    def diagonal(using Piece) = Piece.byOwner(Dir.diagonallyUp, Dir.diagonallyDown)
    def home    (using Piece) = Piece.byOwner(1, 6)
    def goal    (using Piece) = Piece.byOwner(7, 0)
    def distance(using Piece) = if Piece.y == home then 2 else 1
    
    def r_move(using Piece) = Control.moveThis:
      forward.rayFromHere.take(distance).untilPiece
      
    def r_capture(using Piece) = Control.moveThis:
      diagonal.fromHere.ontoEnemy
      
    def r_enpassant(using Piece) = Rule.union:
      Pieces.ofType(Pawn).duringPreviousTurn.moves
        .filter(_.step.abs.y == 2)
        .filter(_.midpoint in (Piece + diagonal))
        .map(m => Piece.move(m.midpoint) |> m.destroy)
      
    def r_promote(using Piece) = Rule.maybe(Piece.y == goal):
      Piece.promote(Rook, Knight, Bishop, Queen)