package boards.games
import boards.GameImports.{*, given}

object Chess extends Game:
  
  given Board: GameBoard = Kernel.box(8, 8)
    .paint(Pattern.Checkered(Colour.Black, Colour.White))
  
  case object Rook extends PieceType.WithTexture("white_rook", "black_rook"):
    def actions(rook: Piece) =
      rook.move(Dir.orthogonal.ray.toEnemy.untilFriendly)
  
  case object Knight extends PieceType.WithTexture("white_knight", "black_knight"):
    def actions(knight: Piece) =
      knight.move(Dir.knight(1, 2).avoidFriendly)
  
  case object Bishop extends PieceType.WithTexture("white_bishop", "black_bishop"):
    def actions(bishop: Piece) =
      bishop.move(Dir.diagonal.ray.toEnemy.untilFriendly)
  
  case object Queen extends PieceType.WithTexture("white_queen", "black_queen"):
    def actions(queen: Piece) =
      queen.move(Dir.octagonal.ray.toEnemy.untilFriendly)
  
  case object King extends PieceType.WithTexture("white_king", "black_king"):
    def actions(king: Piece) =
      
      val MOVE = king.move(Dir.octagonal.avoidFriendly)
      
      val CASTLE = Rule.onCondition(!king.hasMoved):
        Pieces.ofPlayer(king.owner).ofType(Rook)
          .filter: rook =>
            !rook.hasMoved && rook.y == king.y
              && Pieces.regionIsEmpty(king.rayTo(rook).interior)
          .map: rook =>
            val dir = king.directionTo(rook)
            king.move(dir * 2).after(_.move(rook -> (king + dir)))
      .require:
        case (move: Move) ~> _ =>
          !Pieces.ofInactivePlayers.canAttack(move.path, king)
      
      MOVE | CASTLE
  
  case object Pawn extends PieceType.WithTexture("white_pawn", "black_pawn"):
    def actions(pawn: Piece) =
      
      val forward = pawn.byOwner(Dir.up, Dir.down)
      val diagonal = pawn.byOwner(Dir.diagonallyUp, Dir.diagonallyDown)
      val dist = if pawn.hasMoved then 1 else 2
      
      val MOVE = pawn.move(forward.ray.take(dist).untilPiece)
      val CAPTURE = pawn.move(diagonal.ontoEnemy)
      
      val ENPASSANT = Rule.when:
        case Move(enemy, from, to) ~> _ if enemy.is(Pawn)
          && (to - from).y.abs == 2
          && (pawn + diagonal).contains(from.midpoint(to)) =>
            pawn.move(from.midpoint(to)).after(_.destroy(to))
      
      val PROMOTE = Rule.sometimes:
        case Move(piece, _, pos) ~> _ if pos.y == piece.byOwner(7, 0) =>
          piece.replace(Rook, Knight, Bishop, Queen)
      
      (MOVE | CAPTURE | ENPASSANT) |> PROMOTE
  
  val homeRow = Seq(Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook)
  val setup = BoardState(2)
    .place(owner=0)(Board.row(0) -> homeRow, Board.row(1) -> Pawn)
    .place(owner=1)(Board.row(7) -> homeRow, Board.row(6) -> Pawn)
  
  def inCheck(using GameState) =
    Pieces.ofInactivePlayers.canAttack(Pieces.ofActivePlayer.ofType(King))
  
  def rules = Rule.alternatingTurns:
    Pieces.ofActivePlayer.actions
      .require(!inCheck)
      .stopIfImpossible:
        if inCheck then Winner(State.nextPlayer) else Draw