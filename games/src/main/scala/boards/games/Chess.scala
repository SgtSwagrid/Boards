package boards.games

import boards.imports.games.{*, given}
import boards.imports.math.{*, given}
import boards.algebra.shortcuts.{*, given}

object Chess extends Game (
  name = "Chess",
  numPlayers = Seq(2),
  playerNames = Seq("White", "Black"),
  playerColours = Seq(Colour.Chess.Light, Colour.Chess.Dark),
):
  
  override val Board = Kernel.box(8, 8)
    .paint(Pattern.Checkered(Colour.Chess.Dark, Colour.Chess.Light))
  
  object Rook extends
    PieceType.WithTexture(Texture.WhiteRook, Texture.BlackRook),
    PieceType.WithRule(_.move(Dir.orthogonal.ray.toEnemy.untilFriendly))
  
  object Knight extends
    PieceType.WithTexture(Texture.WhiteKnight, Texture.BlackKnight),
    PieceType.WithRule(_.move(Dir.knight(1, 2).avoidFriendly))
  
  object Bishop extends
    PieceType.WithTexture(Texture.WhiteBishop, Texture.BlackBishop),
    PieceType.WithRule(_.move(Dir.diagonal.ray.toEnemy.untilFriendly))
  
  object Queen extends
    PieceType.WithTexture(Texture.WhiteQueen, Texture.BlackQueen),
    PieceType.WithRule(_.move(Dir.octagonal.ray.toEnemy.untilFriendly))
  
  object King extends PieceType.WithTexture(Texture.WhiteKing, Texture.BlackKing):
    
    def actions(king: Piece) =
      
      val r_move = king.move(Dir.octagonal.avoidFriendly)
      
      val r_castle = Rule.when(!king.hasMoved):
        pieces.ofPlayer(king.owner).ofType(Rook)
          .filter: rook =>
            !rook.hasMoved && rook.y == king.y
              && pieces.regionIsEmpty(king.rayTo(rook).interior)
          .map: rook =>
            king.move(Dir.between(king, rook) * 2)
              |> rook.relocate(king + king.directionTo(rook))
      .requireInCase:
        case Following(move: Move) =>
          hypothetically(King.insert(move.path)):
            !pieces.ofInactivePlayers.canMoveTo(move.path)
      
      r_move | r_castle
  
  object Pawn extends PieceType.WithTexture(Texture.WhitePawn, Texture.BlackPawn):
    
    def actions(pawn: Piece) =
      
      val forward = pawn.byOwner(Dir.up, Dir.down)
      val diagonal = pawn.byOwner(Dir.diagonallyUp, Dir.diagonallyDown)
      val dist = if pawn.hasMoved then 1 else 2
      
      val r_move = pawn.move(forward.ray.take(dist).untilPiece)
      val r_capture = pawn.move(diagonal.ontoEnemy)
      
      val r_enpassant = Rule.whenCase:
        case Following(move @ Move(enemy, _, _)) if enemy.is(Pawn)
          && move.step.abs.y == 2
          && (pawn + diagonal).contains(move.midpoint) =>
            pawn.move(move.midpoint) |> enemy.remove
      
      val r_promote = Rule.maybeCase:
        case Following(Move(pawn, _, pos)) if pos.y == pawn.byOwner(7, 0) =>
          pawn.replace(Rook, Knight, Bishop, Queen)
        case _ => skip
      
      (r_move | r_capture | r_enpassant) |> r_promote
  
  val r_setup =
    val homeRow = Seq(Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook)
    Effect.insert(PlayerId(0))(homeRow -> Board.row(0), Pawn -> Board.row(1)) |>
    Effect.insert(PlayerId(1))(homeRow -> Board.row(7), Pawn -> Board.row(6))
  
  def inCheck(using GameState) = pieces.ofInactivePlayers.canCaptureType(King)
  
  override def rules = r_setup |> Rule.alternatingTurns:
    pieces.ofActivePlayer.actions.require(!inCheck)
      ?: Effect.stop(if inCheck then Winner(state.nextPlayer) else Draw)