package boards.games
import boards.GameImports.{*, given}

object Chaturanga extends Game:
  
  given Board: GameBoard = Kernel.box(8, 8)
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
    def actions(padati: Piece) = (
      padati.move(padati.byOwner(Dir.up, Dir.down).ontoEmpty) |
      padati.move(padati.byOwner(Dir.diagonallyUp, Dir.diagonallyDown).ontoEnemy)
    ).after:
      case Move(_, _, to) ~> _ if to.y == padati.byOwner(7, 0) =>
        State.place(to -> Mantri)
      
  val homeRow = Seq(Ratha, Ashva, Gaja, Mantri, Raja, Gaja, Ashva, Ratha)
  val setup = BoardState(2)
    .place(owner=0)(Board.row(0) -> homeRow, Board.row(1) -> Padati)
    .place(owner=1)(Board.row(7) -> homeRow, Board.row(6) -> Padati)
  
  def inCheck(using GameState) =
    Pieces.ofInactivePlayers.canAttack(Pieces.ofActivePlayer.ofType(Raja))
    
  def rules = Rule.alternatingTurns:
    Pieces.ofActivePlayer.actions
      .require(!inCheck)
      .stopIfImpossible(Winner(State.nextPlayer))
      .stopIf(Pieces.ofNextPlayer.forall(_.is(Raja)))(Winner(State.activePlayer))