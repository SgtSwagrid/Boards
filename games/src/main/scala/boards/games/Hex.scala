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

object Hex extends Game:
  
  override val name = "Hex"
  
  val red = Player(0, "Red", Colour.British.NasturcianFlower)
  val blue = Player(1, "Blue", Colour.British.VanadylBlue)
  override val players = Seq(red, blue)

  val size = Property("Size", 7 to 13, default=11)
  override val properties = Seq(size)
  
  override def board = Box(size, size).embedHex
    .paint(Pattern.CheckeredHex(Colour.White, Colour.British.LynxWhite, Colour.British.HintOfPensive))

  override def loop = Rule.alternatingTurns:
   state.board.ontoEmpty.placeFriendly(Hexagon) |> Rule:
     val hex = Pieces.now.sinceTurnStart.created.now.region
     val component = Pieces.ofActivePlayer.now.connectedComponent(hex, Metric.Hex).region
     if state.activePlayer == PlayerId(0) then
       red.winsIf(component.positions.map(_.y).min == 0 && component.positions.map(_.y).max == size.get-1)
     else
       blue.winsIf(component.positions.map(_.x).min == 0 && component.positions.map(_.x).max == size.get-1)

  object Hexagon extends StaticPiece,
    GeometricPiece(Polygon.Hexagon, 1.0F)(Colour.British.NasturcianFlower, Colour.British.VanadylBlue)