package boards.games

import boards.dsl.meta.Game
import boards.dsl.meta.Game.Property
import boards.dsl.meta.PlayerRef.{Player, PlayerRef}
import boards.dsl.pieces.Piece
import boards.dsl.pieces.PieceType.{StaticPiece, TexturedPiece}
import boards.dsl.rules.{Effect, Rule}
import boards.dsl.states.HistoryState
import boards.graphics.{Colour, Texture}
import boards.math.region.EmbeddedRegion.embed
import boards.dsl.Shortcuts.{*, given}
import boards.math.Algebra.{*, given}
import boards.math.region.{Box, Dir}

object TicTacToe extends Game:
  
  override val name = "Tic-Tac-Toe"
  
  val x = Player(0, "X", Colour.British.NasturcianFlower)
  val o = Player(1, "O", Colour.British.VanadylBlue)
  override val players = Seq(x, o)
  
  val size   = Property("Size",   3 to 11, default = 7)
  val target = Property("Target", 3 to 9,  default = 4)
  override val properties = Seq(size, target)
  
  override def board = Box(size, size).embed.fill(Colour.White)
  
  override def loop = Rule.alternatingTurns:
    state.board.ontoEmpty.placeFriendly(Stone) |>
      Effect.stopWhen(streak(state.pieces.latest) >= target)(state.activePlayer.wins)
    .orElseDraw
  
  def streak (using HistoryState, PlayerRef) (piece: Piece): Int =
    Dir.octagonalPairs.map(_.rayFrom(piece, true).whileFriendly.area.toBounded).max
  
  object Stone extends StaticPiece, TexturedPiece(Texture.X, Texture.O)