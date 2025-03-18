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

object Camelot extends Game:

  override val name = "Camelot"

  val white = Player(0, "White", Colour.British.LynxWhite)
  val black = Player(1, "Black", Colour.British.MattPurple)
  override val players = Seq(white, black)

  override def board = Region.stackY (
    Box.rows(2, 8, 10),
    Box(12, 10),
    Box.rows(10, 8, 2),
  ).embed.paint(Pattern.Checkered(Colour.Chess.Dark, Colour.Chess.Light))

  override def setup =
    Man.create(white, (Box.row(6) + Vec(-2, 5)) | (Box.row(4) + Vec(-1, 6))) |>
    Knight.create(white, Vec(-3, 5) | Vec(4, 5) | Vec(-2, 6) | Vec(3, 6)) |>
    Man.create(black, (Box.row(6) + Vec(-2, 10)) | (Box.row(4) + Vec(-1, 9))) |>
    Knight.create(black, Vec(-3, 10) | Vec(4, 10) | Vec(-2, 9) | Vec(3, 9))

  object Man extends TexturedPiece(Texture.WhitePawn, Texture.BlackPawn), StaticPiece

  object Knight extends TexturedPiece(Texture.WhiteKnight, Texture.BlackKnight), StaticPiece