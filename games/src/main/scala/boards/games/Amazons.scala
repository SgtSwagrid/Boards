package boards.games; import boards.imports.all.{*, given}

object Amazons extends Game:
  
  override val name = "Amazons"
  
  val white = Player(0, "White", Colour.British.LynxWhite)
  val black = Player(1, "Black", Colour.British.MattPurple)
  override val players = Seq(white, black)
  
  override val board = Box(10, 10)
    .withLabels(Pattern.Checkered(Colour.Chess.Dark, Colour.Chess.Light))
  
  override def setup =
    Amazon.create(white, VecI(0, 3) | VecI(3, 0) | VecI(6, 0) | VecI(9, 3)) |>
    Amazon.create(black, VecI(0, 6) | VecI(3, 9) | VecI(6, 9) | VecI(9, 6))
  
  override def loop = Rule.alternatingTurns:
    Pieces.ofActivePlayer.actions.orElseStop(State.nextPlayer.wins)
  
  object Amazon extends TexturedPiece(Texture.WhiteQueen, Texture.BlackQueen):
    def rule =
      Control.moveThis(Dir.octagonal.rayFromPiece.untilPiece) |>
      Arrow.placeMine(Dir.octagonal.rayFromPiece.untilPiece)
    
  object Arrow extends StaticPiece, TexturedPiece(Texture.WhiteArrow, Texture.BlackArrow)