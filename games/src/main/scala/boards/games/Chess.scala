package boards.games

import boards.imports.games.{*, given}
import boards.imports.math.{*, given}
import boards.algebra.shortcuts.{*, given}
import boards.math.{Dir, Region}

object Chess extends Game (
  name = "Chess",
  numPlayers = Seq(2),
  playerNames = Seq("White", "Black"),
  playerColours = Seq(Colour.Chess.Light, Colour.Chess.Dark),
):
  
  override val board = Region.box(8, 8)
    .paint(Pattern.Checkered(Colour.Chess.Dark, Colour.Chess.Light))
  
  object Rook extends PieceType.WithTexture(Texture.WhiteRook, Texture.BlackRook):
    def actions(rook: Piece) = rook.move(Dir.orthogonal.rayFromHere.toEnemy.untilFriendly)
  
  object Knight extends PieceType.WithTexture(Texture.WhiteKnight, Texture.BlackKnight):
    def actions(knight: Piece) = knight.move(Dir.knight(1, 2).fromHere.avoidFriendly)
  
  object Bishop extends PieceType.WithTexture(Texture.WhiteBishop, Texture.BlackBishop):
    def actions(bishop: Piece) = bishop.move(Dir.diagonal.rayFromHere.toEnemy.untilFriendly)
  
  object Queen extends PieceType.WithTexture(Texture.WhiteQueen, Texture.BlackQueen):
    def actions(queen: Piece) = queen.move(Dir.octagonal.rayFromHere.toEnemy.untilFriendly)
  
  object King extends PieceType.WithTexture(Texture.WhiteKing, Texture.BlackKing):
    
    def actions(king: Piece) =
      
      val r_move = king.move(Dir.octagonal.fromHere.avoidFriendly)
      
      val r_castle = Rule.when(!king.hasMoved):
        king.fellowPieces.ofType(Rook)
          .filter: rook =>
            !rook.hasMoved &&
            rook.y == king.y &&
            king.rayTo(rook).interior.pieces.isEmpty
          .map: rook =>
            king.move(king + (king.directionTo(rook) * 2)) |>
            rook.relocate(king + king.directionTo(rook))
      .requireInCase:
        case Following(move: Move) =>
          hypothetically(King.insert(move.path)):
            !instances.ofInactivePlayers.canMoveTo(move.path)
      
      r_move | r_castle
  
  object Pawn extends PieceType.WithTexture(Texture.WhitePawn, Texture.BlackPawn):
    
    def actions(pawn: Piece) =
      
      val forward = pawn.byOwner(Dir.up, Dir.down)
      val diagonal = pawn.byOwner(Dir.diagonallyUp, Dir.diagonallyDown)
      val dist = if pawn.hasMoved then 1 else 2
      val r_move = pawn.move(forward.rayFromHere.take(dist).untilPiece)
      val r_capture = pawn.move(diagonal.fromHere.ontoEnemy)
      
      val r_enpassant = Rule.whenCase:
        case Following(move @ Move(enemy, _, _)) if enemy.is(Pawn)
          && move.step.abs.y == 2
          && (pawn ++ diagonal).contains(move.midpoint) =>
            pawn.move(move.midpoint) |> enemy.remove
      
      val r_promote = Rule.maybeCase:
        case Following(Move(pawn, _, pos)) if pos.y == pawn.byOwner(7, 0) =>
          pawn.replace(Rook, Knight, Bishop, Queen)
        case _ => skip
      
      (r_move | r_capture | r_enpassant) |> r_promote
  
  val r_setup =
    val homeRow = Seq(Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook)
    Effect.insert(PlayerId(0))(homeRow -> board.row(0), Pawn -> board.row(1)) |>
    Effect.insert(PlayerId(1))(homeRow -> board.row(7), Pawn -> board.row(6))
  
  override def rules = r_setup |> Rule.alternatingTurns:
    pieces.ofActivePlayer.actions.require(!King.inCheck)
      ?: Effect.stop(if King.inCheck then Winner(state.nextPlayer) else Draw)