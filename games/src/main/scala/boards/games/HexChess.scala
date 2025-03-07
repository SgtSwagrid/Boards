package boards.games

import boards.dsl.meta.Game
import boards.dsl.meta.PlayerRef.Player
import boards.dsl.pieces.Piece
import boards.dsl.pieces.PieceType.{MoveablePiece, TexturedPiece}
import boards.dsl.rules.{Control, Rule}
import boards.dsl.states.HistoryState
import boards.graphics.{Colour, Pattern, Texture}
import boards.math.vector.{Box, Dir}
import boards.math.algebra.Algebra.{*, given}
import boards.math.vector.Embedding.embedHex
import boards.dsl.Shortcuts.{*, given}
import boards.dsl.pieces.PieceView.Pieces
import boards.dsl.states.GameState.Outcome.Draw

object HexChess extends Game:
  
  override val name = "Hex Chess"
  
  val white = Player(0, "White", Colour.British.LynxWhite)
  val black = Player(1, "Black", Colour.British.MattPurple)
  override val players = Seq(white, black)
  
  override def board = Box(8, 8).embedHex
    .paint(Pattern.CheckeredHex(Colour.Red, Colour.Blue, Colour.Green))