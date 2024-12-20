package boards.dsl.pieces

import boards.dsl.meta.PlayerId.PlayerId
import boards.dsl.rules.{Cause, Control, Effect, Rule}
import boards.dsl.shortcuts.piece
import boards.dsl.states.{GameState, HistoryState}
import boards.graphics.Texture
import boards.math.region.Region.HasRegionI

trait PieceType extends PieceFilter:
  
  final def applyBase(pieces: PieceState): PieceView =
    pieces.ofType(this)
  
  def rule: (HistoryState, Piece) ?=> Rule
  
  def texture(piece: Piece): boards.graphics.Texture =
    textures(piece.owner.toInt % textures.size)
  
  def textures: Seq[boards.graphics.Texture] = Seq()
  
  def hash: String = getClass.getSimpleName
  
  def create (
    owner: HistoryState ?=> PlayerId,
    region: HistoryState ?=> HasRegionI,
  ): Effect =
    Effect.create(owner, region, this)
  
  def createMine (
    region: HistoryState ?=> HasRegionI,
  ) (using PlayerId): Effect =
    Effect.createMine(region, this)
  
  def place (
    owner: HistoryState ?=> PlayerId,
    region: HistoryState ?=> HasRegionI,
  ): Rule =
    Control.place(owner, region, this)
  
  def placeMine (
    region: HistoryState ?=> HasRegionI,
  ) (using PlayerId): Rule =
    Control.placeMine(region, this)
  
  def fill (
    owner: HistoryState ?=> PlayerId,
    region: HistoryState ?=> HasRegionI,
  ): Rule =
    Control.fill(owner, region, this)
  
  def fillMine (
    region: HistoryState ?=> HasRegionI,
  ) (using PlayerId): Rule =
    Control.fillMine(region, this)
    
  override def toString = getClass.getSimpleName.dropRight(1)

object PieceType:
  trait TexturedPiece(override val textures: Texture*) extends PieceType
  trait DynamicPiece(behaviour: (HistoryState, Piece) ?=> Rule) extends PieceType:
    def rule: (HistoryState, Piece) ?=> Rule = behaviour
  trait StaticPiece extends PieceType:
    def rule: (HistoryState, Piece) ?=> Rule = Cause.none
  trait MoveablePiece(val region: (HistoryState, Piece) ?=> HasRegionI):
    def rule: (HistoryState, Piece) ?=> Rule = Control.moveThis(region)