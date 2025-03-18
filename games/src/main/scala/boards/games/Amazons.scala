package boards.games

import boards.dsl.meta.{Game, PlayerRef, TurnId}
import boards.dsl.meta.Game.{GameConfig, Property}
import boards.dsl.meta.PlayerRef.{Player, PlayerId, PlayerRef}
import boards.dsl.meta.TurnId.{TurnId, HasTurnId}
import boards.dsl.states.{InstantaneousState, HistoryState, GameState}
import boards.dsl.states.GameState.Outcome
import boards.dsl.states.GameState.Outcome.{Winner, Draw}
import boards.dsl.rules.{Capability, Cause, Control, Effect, Input, Rule}
import boards.dsl.pieces.{Piece, PieceFilter, PieceRef, PieceSet, PieceState, PieceType, PieceUpdate, PieceView}
import boards.dsl.pieces.PieceType.{TexturedPiece, GeometricPiece, DynamicPiece, StaticPiece, MoveablePiece, PieceAppearance}
import boards.dsl.pieces.PieceView.Pieces
import boards.dsl.Shortcuts.{*, given}
import boards.graphics.{Colour, Pattern, Polygon, Texture}

import boards.math.Conversions.{*, given}
import boards.math.{Interval, Number, Rational}
import boards.math.Interval.{IntervalI, IntervalL, IntervalF, IntervalD, IntervalR, given}
import boards.math.vector.{Align, Bounds, Box, Dir, Embedding, Metric, Ray, Region, RegionMap, Vec}
import boards.math.vector.Vec.{VecI, VecL, VecF, VecD, VecR, UVec, UVecI, UVecL, UVecF, UVecD, UVecR, HasVec, given}
import boards.math.vector.Bounds.{BoundsI, BoundsL, BoundsF, BoundsD, BoundsR, HasBounds}
import boards.math.vector.Region.{RegionI, RegionL, given}
import boards.math.vector.RegionMap.{RegionMapI, RegionMapL, HasRegionMap, given}
import boards.math.vector.Embedding.{embed, embedHex}
import boards.math.algebra.Algebra.{*, given}

object Amazons extends Game:
  
  override val name = "Amazons"
  
  val white = Player(0, "White", Colour.British.LynxWhite)
  val black = Player(1, "Black", Colour.British.MattPurple)
  override val players = Seq(white, black)
  
  val size = Property("Size", 8 to 12, default=10)
  override val properties = Seq(size)
  
  override def board = Box(size, size).embed
    .paint(Pattern.Checkered(Colour.Chess.Dark, Colour.Chess.Light))
  
  override def setup =
    Amazon.create(white, VecI(0, 3) | VecI(3, 0) | VecI(size.get-4, 0) | VecI(size.get-1, 3)) |>
    Amazon.create(black, VecI(0, size.get-4) | VecI(3, size.get-1) | VecI(size.get-4, size.get-1) | VecI(size.get-1, size.get-4))
  
  override def loop = Rule.alternatingTurns:
    state.pieces.ofActivePlayer.actions.orElseStop(state.nextPlayer.winner)
  
  object Amazon extends TexturedPiece(Texture.WhiteQueen, Texture.BlackQueen):
    def rule =
      Control.moveThis(Dir.octagonal.rayFromPiece.untilPiece) |>
      Arrow.placeFriendly(Dir.octagonal.rayFromPiece.untilPiece)
    
  object Arrow extends StaticPiece, TexturedPiece(Texture.WhiteArrow, Texture.BlackArrow)