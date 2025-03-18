package boards.dsl.pieces

import boards.dsl.rules.{Cause, Control, Effect, Rule}
import boards.dsl.meta.PlayerRef.PlayerRef
import boards.dsl.pieces.PieceType.PieceAppearance
import boards.dsl.states.{GameState, HistoryState}
import boards.graphics.{Colour, Polygon, Texture}
import boards.math.vector.Region.RegionI

/** Comprises the part of the state of a [[Piece]] over which the [[Game]] implementer has direct control.
  * Acts as a flag to differentiate different kinds of [[Piece]], and can store whatever additional information is needed.
  *
  * Typically, implementations of [[PieceType]] are game-specific case classes or objects.
  */
trait PieceType extends PieceFilter:
  
  final def applyBase (pieces: PieceState): PieceView =
    pieces.ofType(this)
  
  /** The [[Rule]] which governs the behaviour of [[Piece]]s of this [[PieceType]].
    *
    * Generally, [[PieceType]]s should provide their own implementation of this method.
    * A call to this method represents a query for the behaviour of a single [[Piece]] of this [[PieceType]].
    *
    * Note however that the engine never calls this method directly.
    * Instead, the [[Game]] implementer is expected to call [[pieceFilter.actions]] to
    * obtain a single [[Rule]] aggregating the behaviour of all current [[Piece]]s matching the [[pieceFilter]].
    * [[pieceFilter.actions]] will take the union over the [[Rule]]s for each matching [[Piece]],
    * allowing the player to choose which [[Piece]] should be updated.
    * For other behaviour, a custom implementation is required.
    * ([[pieceFilter]] could just be [[Pieces]] to include all existing [[Piece]]s.)
    *
    * In addition to the usual [[HistoryState]],
    * the [[Piece]] itself is also provided to implementations as part of the implicit scope.
    */
  def rule: (state: HistoryState, piece: Piece) ?=> Rule
  
  /** Determines the appropriate [[Texture]] for a [[Piece]] based on its current state. */
  def appearance (piece: Piece): PieceAppearance =
    if piece.isNeutral then appearances.head else
    appearances(piece.owner.playerId.toInt % appearances.size)
  
  def appearances: Seq[PieceAppearance] = Seq()
  
  def hash: String = getClass.getSimpleName
  
  /** Passively create some [[Piece]]s of this [[PieceType]]. */
  def create (
    owner: (state: HistoryState) ?=> PlayerRef,
    region: (state: HistoryState) ?=> RegionI,
  ): Effect =
    Effect.create(owner, region, this)
  
  /** Passively create some [[Piece]]s of this [[PieceType]], belonging to the active player. */
  def createFriendly (
    region: (state: HistoryState) ?=> RegionI,
  ) (using PlayerRef): Effect =
    Effect.createFriendly(region, this)
  
  def createNeutral (
    region: (state: HistoryState) ?=> RegionI,
  ): Effect =
    Effect.createNeutral(region, this)
  
  /** Allow the user to place a [[Piece]] of this [[PieceType]] by clicking. */
  def place (
    owner: (state: HistoryState) ?=> PlayerRef,
    region: (state: HistoryState) ?=> RegionI,
  ): Rule =
    Control.place(owner, region, this)
  
  /** Allow the user to place a [[Piece]] of this [[PieceType]], belonging to the active player. */
  def placeFriendly (
    region: (state: HistoryState) ?=> RegionI,
  ) (using PlayerRef): Rule =
    Control.placeFriendly(region, this)
  
  def placeNeutral (
    region: (state: HistoryState) ?=> RegionI,
  ): Rule =
    Control.placeNeutral(region, this)
  
  /** Fill a [[RegionI]] with [[Piece]]s of this [[PieceType]]. */
  def fill (
    owner: (state: HistoryState) ?=> PlayerRef,
    region: (state: HistoryState) ?=> RegionI,
  ): Rule =
    Control.fill(owner, region, this)
  
  /** Fill a [[RegionI]] with [[Piece]]s of this [[PieceType]], belonging to the active player. */
  def fillFriendly (
    region: (state: HistoryState) ?=> RegionI,
  ) (using PlayerRef): Rule =
    Control.fillFriendly(region, this)
  
  def fillNeutral (
    region: (state: HistoryState) ?=> RegionI,
  ): Rule =
    Control.fillNeutral(region)
    
  override def toString = getClass.getSimpleName.dropRight(1)

object PieceType:
  
  enum PieceAppearance:
    case Textured (texture: Texture)
    case Shape (shape: Polygon, colour: Colour, scale: Float = 0.5F)
  
  /** A [[PieceType]] with static, possibly owner-dependent [[Texture]]s. */
  trait TexturedPiece (textures: Texture*) extends PieceType:
    override val appearances = textures.map(PieceAppearance.Textured.apply)
    
  trait GeometricPiece (shape: Polygon, scale: Float = 0.5F) (colours: Colour*) extends PieceType:
    override val appearances = colours.map(colour => PieceAppearance.Shape(shape, colour, scale))
  
  /** A [[PieceType]] which may act in an arbitrary way. */
  trait DynamicPiece (behaviour: (HistoryState, Piece) ?=> Rule) extends PieceType:
    def rule: (HistoryState, Piece) ?=> Rule = behaviour
    
  /** A [[PieceType]] which never moves or acts at all, for static obstacles. */
  trait StaticPiece extends PieceType:
    def rule: (HistoryState, Piece) ?=> Rule = Cause.none
    
  /** A [[PieceType]] which acts only by simple movement. */
  trait MoveablePiece (val region: (HistoryState, Piece) ?=> RegionI):
    def rule: (HistoryState, Piece) ?=> Rule = Control.moveThis(region)