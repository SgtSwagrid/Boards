package boards.games

import boards.dsl.meta.Game
import boards.dsl.meta.PlayerRef.Player
import boards.dsl.pieces.PieceType.{StaticPiece, TexturedPiece}
import boards.dsl.rules.{Control, Rule}
import boards.graphics.{Colour, Pattern, Texture}
import boards.math.region.{Box, Dir}
import boards.math.region.EmbeddedRegion.embed
import boards.math.region.Vec.VecI
import boards.dsl.Shortcuts.{*, given}
import boards.dsl.meta.Game.Property

object Amazons extends Game:
  
  override val name = "Amazons"
  
  val white = Player(0, "White", Colour.British.LynxWhite)
  val black = Player(1, "Black", Colour.British.MattPurple)
  override val players = Seq(white, black)
  
  val size = Property("Size", 8 to 12, default = 10)
  override val properties = Seq(size)
  
  override def board = Box(size, size).embed
    .paint(Pattern.Checkered(Colour.Chess.Dark, Colour.Chess.Light))
  
  override def setup =
    Amazon.create(white, VecI(0, 3) | VecI(3, 0) | VecI(size-4, 0) | VecI(size-1, 3)) |>
    Amazon.create(black, VecI(0, size-4) | VecI(3, size-1) | VecI(size-4, size-1) | VecI(size-1, size-4))
  
  override def loop = Rule.alternatingTurns:
    state.pieces.ofActivePlayer.actions.orElseStop(state.nextPlayer.wins)
  
  object Amazon extends TexturedPiece(Texture.WhiteQueen, Texture.BlackQueen):
    def rule =
      Control.moveThis(Dir.octagonal.rayFromPiece.untilPiece) |>
      Arrow.placeFriendly(Dir.octagonal.rayFromPiece.untilPiece)
    
  object Arrow extends StaticPiece, TexturedPiece(Texture.WhiteArrow, Texture.BlackArrow)