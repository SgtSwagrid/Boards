package boards.algebra.state

import boards.algebra.rules.Rule
import boards.algebra.rules.{Generator, Rule}
import boards.algebra.state.GameState
import boards.algebra.state.Piece.*
import boards.algebra.Game.PlayerId
import boards.algebra.shortcuts.pieces
import boards.imports.games.{*, given}
import boards.imports.math.{*, given}

import java.util.Random
import scala.annotation.targetName
import scala.reflect.ClassTag

/**
 * A game piece that exists somewhere on the game board.
 *
 * @param pieceType A flag indicating the type of piece this is.
 * @param position The current location of the piece on the game board.
 * @param owner The player ID (turn position) of the player who owns this piece.
 * @param hasMoved Whether this piece has ever been moved in the game's history,
 *                 be it actively or passively.
 * @param id An ID which uniquely identifies this piece.
 *           The ID remains the same even after the piece is moved.
 */
case class Piece (
  pieceType: PieceType,
  position: VecI,
  owner: PlayerId = PlayerId(-1),
  hasMoved: Boolean = false,
  id: Int = (new Random).nextInt(),
) extends PartialPiece.WithPosition, PartialPiece.WithOwner, Vec[Int]:
  
  given PlayerId = owner
  export pieceType.{actions as _, *}
  export position.{apply, components}
  override val dim: Int = position.dim
  
  def actions: Rule = Rule.switch(pieceType.actions(this)(using summon[GameState], owner))
  
  def ownedBy(player: PlayerId): Boolean = owner == player
  def byOwner[X](x: X*): X = x(owner.toInt)
  infix def is(pieceType: Any): Boolean = this.pieceType == pieceType
  def is[Q](using C: ClassTag[Q]): Boolean =
    pieceType.getClass == C.runtimeClass
    
  def move(to: VecI ?=> RegionI): Rule =
    Generator.move(position -> to(using position))
  def replace(pieces: PieceType*): Rule =
    Generator.place(pieces -> position)
  def destroy: Rule =
    Generator.destroy(position)
    
  def relocate(to: VecI ?=> VecI): Rule =
    Effect.relocate(position -> to(using position))
  def substitute(piece: PieceType): Rule =
    Effect.insert(owner)(piece -> position)
  def remove: Rule =
    Effect.remove(position)
    
  def texture: Texture = pieceType.texture(this)
  
  /** The set of all current pieces with the same owner as this one. */
  def fellowPieces(using pieces: PieceSet): PieceSet = pieces.ofPlayer(owner)
  
  override def toString = pieceType.getClass.getSimpleName.dropRight(1)
  
object Piece:
  
  trait PieceType:
    def actions(piece: Piece): (GameState, PlayerId) ?=> Rule
    def texture(piece: Piece): boards.graphics.Texture =
      textures(piece.owner.toInt % textures.size)
    def textures: Seq[boards.graphics.Texture] = Seq()
    def hash: String = getClass.getSimpleName
    
    def place(owner: PlayerId)(pos: RegionI*): Rule =
      Generator.place(owner)(this -> pos.reduce(_ | _))
    
    def place(pos: RegionI*)(using PlayerId): Rule =
      Generator.place(this -> pos.reduce(_ | _))
      
    def insert(owner: PlayerId)(pos: RegionI*): Rule =
      Effect.insert(owner)(this -> pos.reduce(_ | _))
      
    def insert(pos: RegionI*)(using PlayerId): Rule =
      Effect.insert(this -> pos.reduce(_ | _))
      
    /**
     * Whether an inactive player is currently able to move a piece so as to
     * capture a piece belonging to the active player of the given type.
     */
    def inCheck(using GameState): Boolean =
      import boards.algebra.shortcuts.{*, given}
      summon[PieceSet].ofInactivePlayers.canCaptureType(this)
    
    /** The set of all current pieces with this type, belonging to all players. */
    def instances(using PieceSet): PieceSet =
      import boards.algebra.shortcuts.{*, given}
      summon[PieceSet].ofType(this)
    
  object PieceType:
    trait WithTexture(override val textures: Texture*) extends PieceType
    trait WithRule(val rule: Piece => (GameState, PlayerId) ?=> Rule) extends PieceType:
      def actions(piece: Piece): (GameState, PlayerId) ?=> Rule = rule(piece)
    abstract class Immobile extends WithRule(_ => Rule.none)
      
  trait PartialPiece:
    def pieceType: PieceType
    
  object PartialPiece:
    trait WithPosition extends PartialPiece:
      def position: VecI
    trait WithOwner extends PartialPiece:
      def owner: PlayerId