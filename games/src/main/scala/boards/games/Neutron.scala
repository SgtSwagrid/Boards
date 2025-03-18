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

object Neutron extends Game:
  
  override val name = "Neutron"
  
  val blue = Player(0, "Blue", Colour.British.MattPurple)
  val red  = Player(1, "Red", Colour.British.HarleyDavidsonOrange)
  override val players = Seq(blue, red)
  
  override def board = Box(5, 5).embed(border=0.05F).paint:
    case Vec(_, 0) => blue.colour.lighten(50).mix(Colour.French.AuroraGreen, 0.3)
    case Vec(_, 4) => red.colour.lighten(50).mix(Colour.French.AuroraGreen, 0.3)
    case _         => Colour.French.AuroraGreen
  
  override def setup =
    Ball.create(blue, board.row(0)) |>
    Ball.create(red,  board.row(4)) |>
    NeutronPiece.createNeutral(Vec(2, 2))
    
  override def loop = Rule:
    Ball.ofActivePlayer.actions |> Effect.endTurn |>
    Rule.alternatingTurns:
      NeutronPiece.actions.orElse(state.nextPlayer.wins) |>
      blue.winsIf(board.row(0).pieces.ofType(NeutronPiece).nonEmpty) |>
      red .winsIf(board.row(4).pieces.ofType(NeutronPiece).nonEmpty) |>
      Ball.ofActivePlayer.actions
    
  object Ball extends
    GeometricPiece(Polygon.Hexagon)(Colour.British.MattPurple, Colour.British.HarleyDavidsonOrange),
    MoveablePiece(Dir.octagonal.rayFromHere.untilPiece.whileInBounds.last)
    
  object NeutronPiece extends
    GeometricPiece(Polygon.Hexagon)(Colour.British.NanohanachaGold),
    MoveablePiece(Dir.octagonal.rayFromHere.untilPiece.whileInBounds.last)