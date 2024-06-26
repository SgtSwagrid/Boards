package boards.games
import boards.GameImports.{*, given}

object Chess extends Game:
  
  val Board = Kernel.box(8, 8)
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
            king.move(dir * 2).after(rook.relocate(king + dir))
      .require:
        case Following(move: Move) =>
          hypothetically(Pieces.insert(king -> move.path)):
            !Pieces.ofInactivePlayers.canAttack(move.path)
      
      MOVE | CASTLE
  
  case object Pawn extends PieceType.WithTexture("white_pawn", "black_pawn"):
    def actions(pawn: Piece) =
      
      val forward = pawn.byOwner(Dir.up, Dir.down)
      val diagonal = pawn.byOwner(Dir.diagonallyUp, Dir.diagonallyDown)
      val dist = if pawn.hasMoved then 1 else 2
      
      val MOVE = pawn.move(forward.ray.take(dist).untilPiece)
      val CAPTURE = pawn.move(diagonal.ontoEnemy)
      
      val ENPASSANT = Rule.when:
        case Following(move @ Move(enemy, _, _)) if enemy.is(Pawn)
          && move.step.y.abs == 2
          && (pawn + diagonal).contains(move.midpoint) =>
            pawn.move(move.midpoint).after(enemy.remove)
      
      val PROMOTE = Rule.sometimes:
        case Following(Move(pawn, _, pos)) if pos.y == pawn.byOwner(7, 0) =>
          pawn.replace(Rook, Knight, Bishop, Queen)
      
      (MOVE | CAPTURE | ENPASSANT) |> PROMOTE
  
  val homeRow = Seq(Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook)
  def setup = Pieces
    .insert(owner=0)(homeRow -> Board.row(0), Pawn -> Board.row(1))
    .insert(owner=1)(homeRow -> Board.row(7), Pawn -> Board.row(6))
  
  def inCheck(using GameState) =
    Pieces.ofInactivePlayers.canAttack(Pieces.ofActivePlayer.ofType(King))
  
  def rules = Rule.alternatingTurns:
    Pieces.ofActivePlayer.actions
      .require(!inCheck)
      .stopIfImpossible:
        if inCheck then Winner(State.nextPlayer) else Draw