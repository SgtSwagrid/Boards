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

object Chaturanga extends Game:
  
  override val name = "Chaturanga"
  
  val white = Player(0, "White", Colour.British.LynxWhite)
  val black = Player(1, "Black", Colour.British.MattPurple)
  override val players = Seq(white, black)
  
  override def board = Box(8, 8).embed
    .paint(Pattern.Checkered(Colour.Chess.Dark, Colour.Chess.Light))
  
  override def setup =
    board.row(7).create(black, Ratha, Ashva, Gaja, Mantri, Raja, Gaja, Ashva, Ratha) |>
    board.row(6).create(black, Padati) |>
    board.row(1).create(white, Padati) |>
    board.row(0).create(white, Ratha, Ashva, Gaja, Mantri, Raja, Gaja, Ashva, Ratha)
    
  override def loop = Rule.alternatingTurns:
    Pieces.ofActivePlayer.actions.require(!inCheck).orElse(state.nextPlayer.wins) |>
    state.activePlayer.winsIf(Pieces.ofNextPlayer.now.forall(_ is Raja))
  
  def inCheck (using HistoryState) = Raja.ofActivePlayer.inCheck
  
  object Ratha extends
    TexturedPiece(Texture.WhiteRatha, Texture.BlackRatha),
    MoveablePiece(Dir.orthogonal.rayFromHere.toEnemy.untilFriendly)
  
  object Ashva extends
    TexturedPiece(Texture.WhiteAshva, Texture.BlackAshva),
    MoveablePiece(Dir.knight(1, 2).fromHere.avoidFriendly)
  
  object Gaja extends
    TexturedPiece(Texture.WhiteGaja, Texture.BlackGaja),
    MoveablePiece((Dir.diagonal * 2).fromHere.avoidFriendly)
  
  object Mantri extends
    TexturedPiece(Texture.WhiteMantri, Texture.BlackMantri),
    MoveablePiece(Dir.diagonal.fromHere.avoidFriendly)
  
  object Raja extends
    TexturedPiece(Texture.WhiteRaja, Texture.BlackRaja),
    MoveablePiece(Dir.octagonal.fromHere.avoidFriendly)
  
  object Padati extends TexturedPiece(Texture.WhitePadati, Texture.BlackPadati):
    def rule =
      val r_move = piece.move(piece.byOwner(Dir.up, Dir.down).fromHere.ontoEmpty)
      val r_capture = piece.move(piece.byOwner(Dir.diagonallyUp, Dir.diagonallyDown).fromHere.ontoEnemy)
      val r_promote = Rule.maybe(piece.y == piece.byOwner(7, 0))(piece.replace(Mantri))
      (r_move | r_capture) |> r_promote