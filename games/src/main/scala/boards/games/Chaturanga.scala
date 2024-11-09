package boards.games

import boards.imports.games.{*, given}
import boards.imports.math.{*, given}
import boards.algebra.shortcuts.{*, given}
import boards.math.{Dir, Region}

object Chaturanga extends Game (
  name = "Chaturanga",
  numPlayers = Seq(2),
  playerNames = Seq("White", "Black"),
  playerColours = Seq(Colour.White, Colour.Black)
):
  
  override val board = Region.box(8, 8)
    .paint(Pattern.Checkered(Colour.Black, Colour.White))
  
  object Ratha extends PieceType.WithTexture(Texture.WhiteRatha, Texture.BlackRatha):
    def actions(ratha: Piece) = ratha.move(Dir.orthogonal.rayFromHere.toEnemy.untilFriendly)
  
  object Ashva extends PieceType.WithTexture(Texture.WhiteAshva, Texture.BlackAshva):
    def actions(ashva: Piece) = ashva.move(Dir.knight(1, 2).fromHere.avoidFriendly)
  
  object Gaja extends PieceType.WithTexture(Texture.WhiteGaja, Texture.BlackGaja):
    def actions(gaja: Piece) = gaja.move((Dir.diagonal * 2).fromHere.avoidFriendly)
  
  object Mantri extends PieceType.WithTexture(Texture.WhiteMantri, Texture.BlackMantri):
    def actions(mantri: Piece) = mantri.move(Dir.diagonal.fromHere.avoidFriendly)
      
  object Raja extends PieceType.WithTexture(Texture.WhiteRaja, Texture.BlackRaja):
    def actions(raja: Piece) = raja.move(Dir.octagonal.fromHere.avoidFriendly)
      
  object Padati extends PieceType.WithTexture(Texture.WhitePadati, Texture.BlackPadati):
    def actions(padati: Piece) =
      val r_move =
        padati.move(padati.byOwner(Dir.up, Dir.down).fromHere.ontoEmpty) |
        padati.move(padati.byOwner(Dir.diagonallyUp, Dir.diagonallyDown).fromHere.ontoEnemy)
      val r_promote = Rule.whenCase:
        case Following(Move(piece, _, to)) if to.y == padati.byOwner(7, 0) =>
          piece.replace(Mantri)
      r_move |> r_promote
  
  val r_setup =
    val homeRow = Seq(Ratha, Ashva, Gaja, Mantri, Raja, Gaja, Ashva, Ratha)
    insert(PlayerId(0))(homeRow -> board.row(0), Padati -> board.row(1)) |>
    insert(PlayerId(1))(homeRow -> board.row(7), Padati -> board.row(6))
  
  def inCheck(using GameState) = pieces.ofInactivePlayers.canCaptureType(Raja)
    
  override def rules = r_setup |> alternatingTurns:
    pieces.ofActivePlayer.actions
      .require(!inCheck).orElseIfImpossible(stop(Winner(state.nextPlayer)))
      .thenIf(pieces.ofNextPlayer.forall(_.is(Raja)))(stop(Winner(state.activePlayer)))