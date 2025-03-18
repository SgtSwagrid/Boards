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

object Breakthrough extends Game:
  
  override val name = "Breakthrough"
  
  val white = Player(0, "White", Colour.British.LynxWhite)
  val black = Player(1, "Black", Colour.British.MattPurple)
  override val players = Seq(white, black)
  
  val size = Property("Size", 4 to 10, default=8)
  override val properties = Seq(size)
  
  override def board = Box(size, size).embed
    .paint(Pattern.Checkered(Colour.Chess.Dark, Colour.Chess.Light))
  
  override def setup =
    (board.row(0) | board.row(1)).create(white, Pawn) |>
    (board.row(size.get-1) | board.row(size.get-2)).create(black, Pawn)
    
  def goal (using HistoryState) = byActivePlayer(board.row(size.get-1), board.row(0))
    
  override def loop = Rule.alternatingTurns:
    Pieces.ofActivePlayer.now.actions |>
    state.activePlayer.winsIf:
      Pieces.ofNextPlayer.now.isEmpty ||
      Pieces.now.sinceTurnStart.hasMovedTo(goal)
    
  object Pawn extends TexturedPiece(Texture.WhitePawn, Texture.BlackPawn):
    def rule = Control.moveThis(forward.fromHere.ontoEmpty | diagonal.fromHere.ontoEnemy)
    def forward (using piece: Piece) = piece.byOwner(Dir.up | Dir.diagonallyUp, Dir.down | Dir.diagonallyDown)
    def diagonal (using piece: Piece) = piece.byOwner(Dir.diagonallyUp, Dir.diagonallyDown)