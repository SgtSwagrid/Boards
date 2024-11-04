package boards.games

import boards.imports.games.{*, given}
import boards.imports.math.{*, given}
import boards.algebra.shortcuts.{*, given}
import boards.games.Chess.Pawn

object Chaturanga extends Game (
  name = "Chaturanga",
  numPlayers = Seq(2),
  playerNames = Seq("White", "Black"),
  playerColours = Seq(Colour.White, Colour.Black)
):
  
  override val Board = Kernel.box(8, 8)
    .paint(Pattern.Checkered(Colour.Black, Colour.White))
  
  object Ratha extends
    PieceType.WithTexture(Texture.WhiteRatha, Texture.BlackRatha),
    PieceType.WithRule(_.move(Dir.orthogonal.ray.toEnemy.untilFriendly))
  
  object Ashva extends
    PieceType.WithTexture(Texture.WhiteAshva, Texture.BlackAshva),
    PieceType.WithRule(_.move(Dir.knight(1, 2).avoidFriendly))
  
  object Gaja extends
    PieceType.WithTexture(Texture.WhiteGaja, Texture.BlackGaja),
    PieceType.WithRule(_.move((Dir.diagonal * 2).avoidFriendly))
  
  object Mantri extends
    PieceType.WithTexture(Texture.WhiteMantri, Texture.BlackMantri),
    PieceType.WithRule(_.move(Dir.diagonal.avoidFriendly))
      
  object Raja extends
    PieceType.WithTexture(Texture.WhiteRaja, Texture.BlackRaja),
    PieceType.WithRule(_.move(Dir.octagonal.avoidFriendly))
      
  object Padati
    extends PieceType.WithTexture(Texture.WhitePadati, Texture.BlackPadati):
    def actions(padati: Piece) =
      val r_move =
        padati.move(padati.byOwner(Dir.up, Dir.down).ontoEmpty) |
        padati.move(padati.byOwner(Dir.diagonallyUp, Dir.diagonallyDown).ontoEnemy)
      val r_promote = whenCase:
        case Following(Move(piece, _, to)) if to.y == padati.byOwner(7, 0) =>
          piece.replace(Mantri)
      r_move |> r_promote
  
  val r_setup =
    val homeRow = Seq(Ratha, Ashva, Gaja, Mantri, Raja, Gaja, Ashva, Ratha)
    insert(PlayerId(0))(homeRow -> Board.row(0), Padati -> Board.row(1)) |>
    insert(PlayerId(1))(homeRow -> Board.row(7), Padati -> Board.row(6))
  
  def inCheck(using GameState) = pieces.ofInactivePlayers.canCaptureType(Raja)
    
  override def rules = r_setup |> alternatingTurns:
    pieces.ofActivePlayer.actions
      .require(!inCheck).orElseIfImpossible(stop(Winner(state.nextPlayer)))
      .thenIf(pieces.ofNextPlayer.forall(_.is(Raja)))(stop(Winner(state.now.activePlayer)))