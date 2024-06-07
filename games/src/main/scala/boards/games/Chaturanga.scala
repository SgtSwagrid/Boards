package boards.games
import boards.GameImports.{*, given}

object Chaturanga extends Game:
  
  val Board = Kernel.box(8, 8)
    .paint(Pattern.Checkered(Colour.Black, Colour.White))
  
  case object Ratha extends PieceType.WithTexture("white_cratha", "black_ratha"):
    def actions(ratha: Piece) =
      ratha.move(Dir.orthogonal.ray.toEnemy.untilFriendly)
  
  case object Ashva extends PieceType.WithTexture("white_ashva", "black_ashva"):
    def actions(ashva: Piece) =
      ashva.move(Dir.knight(1, 2).avoidFriendly)
  
  case object Gaja extends PieceType.WithTexture("white_gaja", "black_gaja"):
    def actions(gaja: Piece) =
      gaja.move((Dir.diagonal * 2).avoidFriendly)
  
  case object Mantri extends PieceType.WithTexture("white_mantri", "black_matri"):
    def actions(mantri: Piece) =
      mantri.move(Dir.diagonal.avoidFriendly)
      
  case object Raja extends PieceType.WithTexture("white_raja", "black_raja"):
    def actions(raja: Piece) =
      raja.move(Dir.octagonal.avoidFriendly)
      
  case object Padati extends PieceType.WithTexture("white_padati", "black_padati"):
    def actions(padati: Piece) =
      val MOVE =
        padati.move(padati.byOwner(Dir.up, Dir.down).ontoEmpty) |
        padati.move(padati.byOwner(Dir.diagonallyUp, Dir.diagonallyDown).ontoEnemy)
      MOVE.after:
        case Following(Move(_, _, to)) if to.y == padati.byOwner(7, 0) =>
          Pieces.insert(Mantri -> to)
      
  val homeRow = Seq(Ratha, Ashva, Gaja, Mantri, Raja, Gaja, Ashva, Ratha)
  val setup = Pieces
    .insert(owner=0)(homeRow -> Board.row(0), Padati -> Board.row(1))
    .insert(owner=1)(homeRow -> Board.row(7), Padati -> Board.row(6))
  
  def inCheck(using GameState) =
    Pieces.ofInactivePlayers.canAttack(Pieces.ofActivePlayer.ofType(Raja))
    
  def rules = Rule.alternatingTurns:
    Pieces.ofActivePlayer.actions
      .require(!inCheck)
      .stopIfImpossible(Winner(State.nextPlayer))
      .stopIf(Pieces.ofNextPlayer.forall(_.is(Raja)))(Winner(State.activePlayer))