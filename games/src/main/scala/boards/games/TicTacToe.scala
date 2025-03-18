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

object TicTacToe extends Game:
  
  override val name = "Tic-Tac-Toe"
  
  val x = Player(0, "X", Colour.British.NasturcianFlower)
  val o = Player(1, "O", Colour.British.VanadylBlue)
  override val players = Seq(x, o)
  
  val size   = Property("Size",   3 to 11, default=7)
  val target = Property("Target", 3 to 9,  default=4)
  override val properties = Seq(size, target)
  
  override def board = Box(size, size).embed(border=0.05F)
    .paintSolid(Colour.American.SourLemon)
  
  override def loop = Rule.alternatingTurns:
    board.ontoEmpty.placeFriendly(Stone).orElseDraw |>
    Effect.stopWhen(streak(state.pieces.latest) >= target)(state.activePlayer.winner)
  
  def streak (using HistoryState, PlayerRef) (piece: Piece): Int =
    Dir.octagonalPairs.map(_.rayFrom(piece, true).whileFriendly.area).max.toFinite
  
  object Stone extends StaticPiece, TexturedPiece(Texture.X, Texture.O)