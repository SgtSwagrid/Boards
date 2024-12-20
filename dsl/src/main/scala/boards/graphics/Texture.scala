package boards.graphics

case class Texture(file: String)

case object Texture:
  
  val WhitePawn   = Texture("chess/white_pawn.svg")
  val WhiteRook   = Texture("chess/white_rook.svg")
  val WhiteKnight = Texture("chess/white_knight.svg")
  val WhiteBishop = Texture("chess/white_bishop.svg")
  val WhiteQueen  = Texture("chess/white_queen.svg")
  val WhiteKing   = Texture("chess/white_king.svg")
  val BlackPawn   = Texture("chess/black_pawn.svg")
  val BlackRook   = Texture("chess/black_rook.svg")
  val BlackKnight = Texture("chess/black_knight.svg")
  val BlackBishop = Texture("chess/black_bishop.svg")
  val BlackQueen  = Texture("chess/black_queen.svg")
  val BlackKing   = Texture("chess/black_king.svg")
  
  val WhiteRatha  = Texture("chess/white_rook.svg")
  val WhiteAshva  = Texture("chess/white_knight.svg")
  val WhiteGaja   = Texture("chess/white_bishop.svg")
  val WhiteMantri = Texture("chess/white_queen.svg")
  val WhiteRaja   = Texture("chess/white_king.svg")
  val WhitePadati = Texture("chess/white_pawn.svg")
  val BlackRatha  = Texture("chess/black_rook.svg")
  val BlackAshva  = Texture("chess/black_knight.svg")
  val BlackGaja   = Texture("chess/black_bishop.svg")
  val BlackMantri = Texture("chess/black_queen.svg")
  val BlackRaja   = Texture("chess/black_king.svg")
  val BlackPadati = Texture("chess/black_pawn.svg")