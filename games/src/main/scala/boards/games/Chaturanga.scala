package boards.games

import boards.imports.games.{*, given}
import boards.algebra.Shortcuts.{*, given}

object Chaturanga extends Game (
  name = "Chaturanga",
  numPlayers = Seq(2),
  playerNames = Seq("White, Black"),
  playerColours = Seq(Colour.White, Colour.Black)
):
  
  override val Board = Kernel.box(8, 8)
    .paint(Pattern.Checkered(Colour.Black, Colour.White))
  
  case object Ratha extends
    PieceType.WithTexture(Texture.WhiteRatha, Texture.BlackRatha),
    PieceType.WithRule(_.move(Dir.orthogonal.ray.toEnemy.untilFriendly))
  
  case object Ashva extends
    PieceType.WithTexture(Texture.WhiteAshva, Texture.BlackAshva),
    PieceType.WithRule(_.move(Dir.knight(1, 2).avoidFriendly))
  
  case object Gaja extends
    PieceType.WithTexture(Texture.WhiteGaja, Texture.BlackGaja),
    PieceType.WithRule(_.move((Dir.diagonal * 2).avoidFriendly))
  
  case object Mantri extends
    PieceType.WithTexture(Texture.WhiteMantri, Texture.BlackMantri),
    PieceType.WithRule(_.move(Dir.diagonal.avoidFriendly))
      
  case object Raja extends
    PieceType.WithTexture(Texture.WhiteRaja, Texture.BlackRaja),
    PieceType.WithRule(_.move(Dir.octagonal.avoidFriendly))
      
  case object Padati
    extends PieceType.WithTexture(Texture.WhitePadati, Texture.BlackPadati):
    def actions(padati: Piece) =
      val MOVE =
        padati.move(padati.byOwner(Dir.up, Dir.down).ontoEmpty) |
        padati.move(padati.byOwner(Dir.diagonallyUp, Dir.diagonallyDown).ontoEnemy)
      MOVE.after:
        case Following(Move(_, _, to)) if to.y == padati.byOwner(7, 0) =>
          Pieces.insert(Mantri -> to)
      
  val homeRow = Seq(Ratha, Ashva, Gaja, Mantri, Raja, Gaja, Ashva, Ratha)
  override def setup(numPlayers: Int) = Pieces
    .insert(owner=0)(homeRow -> Board.row(0), Padati -> Board.row(1))
    .insert(owner=1)(homeRow -> Board.row(7), Padati -> Board.row(6))
  
  def inCheck(using GameState) =
    Pieces.ofInactivePlayers.canAttack(Pieces.ofActivePlayer.ofType(Raja))
    
  override def rules = Rule.alternatingTurns:
    Pieces.ofActivePlayer.actions
      .require(!inCheck)
      .stopIfImpossible(Winner(State.nextPlayer))
      .stopIf(Pieces.ofNextPlayer.forall(_.is(Raja)))(Winner(State.now.activePlayer))