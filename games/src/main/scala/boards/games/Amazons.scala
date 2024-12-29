package boards.games; import boards.imports.all.{*, given}

object Amazons extends Game (
  name = "Amazons",
  numPlayers = Seq(2),
  playerNames = Seq("White", "Black"),
  playerColours = Seq(Colour.British.LynxWhite, Colour.British.MattPurple),
):
  
  val board = Box(10, 10)
    .withLabels(Pattern.Checkered(Colour.Chess.Dark, Colour.Chess.Light))
  
  val Seq(white, black) = Seq(0, 1).map(PlayerId.apply)
  
  object Amazon extends TexturedPiece(Texture.WhiteQueen, Texture.BlackQueen):
    def rule = r_move |> r_shoot
    def r_move(using Piece, HistoryState) = Control.moveThis(Dir.octagonal.rayFromPiece.untilPiece)
    def r_shoot(using Piece, HistoryState) = Arrow.placeMine(Dir.octagonal.rayFromPiece.untilPiece)
    
  object Arrow extends StaticPiece, TexturedPiece(Texture.WhiteArrow, Texture.BlackArrow)
  
  def rules = r_setup |> r_loop
  
  def r_setup =
    val r_white = Amazon.create(white, VecI(0, 3) | VecI(3, 0) | VecI(6, 0) | VecI(9, 3))
    val r_black = Amazon.create(black, VecI(0, 6) | VecI(3, 9) | VecI(6, 9) | VecI(9, 6))
    r_white |> r_black
    
  def r_loop = Rule.alternatingTurns:
    Pieces.ofActivePlayer.actions.orElseStop(State.nextPlayer.wins)