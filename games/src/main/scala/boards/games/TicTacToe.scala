package boards.games; import boards.imports.all.{*, given}

object TicTacToe extends Game:
  
  override val name = "Tic-Tac-Toe"
  
  val x = Player(0, "X", Colour.British.NasturcianFlower)
  val o = Player(1, "O", Colour.British.VanadylBlue)
  override val players = Seq(x, o)
  
  val size   = Game.Property("Size",   3 to 11, default = 7)
  val target = Game.Property("Target", 3 to 11, default = 5)
  override val properties = Seq(size, target)
  
  override def setup = Effect:
    Box(size.get, size.get).uniformLabels(Colour.White).use
  
  override def loop = Rule.alternatingTurns:
    State.board.ontoEmpty.placeMine(Stone) |>
      Effect.stopWhen(streak(Pieces.now.latest) >= target.get)(State.activePlayer.wins)
    .orElseDraw
  
  def streak(using HistoryState, PlayerRef)(piece: Piece) =
    Dir.octagonalPairs.map(_.rayFrom(piece, true).whileFriendly.area.asFinite).max
  
  object Stone extends StaticPiece, TexturedPiece(Texture.X, Texture.O)