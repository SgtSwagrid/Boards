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

object Clobber extends Game:
  
  override val name = "Clobber"
  
  val white = Player(0, "White", Colour.British.LynxWhite)
  val black = Player(1, "Black", Colour.British.MattPurple)
  override val players = Seq(white, black)
  
  val width = Property("Width", 4 to 10, default=5)
  val height = Property("Height", 4 to 10, default=6)
  override val properties = Seq(width, height)
  
  override def board = Box(width, height).embed
    .paint(Pattern.Checkered(Colour.Chess.Dark, Colour.Chess.Light))
  
  override def setup =
    Ball.create(white, board.filter(_.sum % 2 == 1)) |>
    Ball.create(black, board.filter(_.sum % 2 == 0))
    
  override def loop = Rule.alternatingTurns:
    Ball.ofActivePlayer.actions.orElse(state.nextPlayer.wins)
  
  object Ball extends
    GeometricPiece(Polygon.Hexagon)(white.colour, black.colour),
    MoveablePiece(Dir.orthogonal.fromHere.ontoEnemy)