package boards.algebra

import boards.imports.games.{*, given}
import boards.imports.math.{*, given}

import Piece.{PartialPiece, PieceType}

import scala.annotation.targetName
import scala.reflect.ClassTag
import java.util.Random

case class Piece (
  pieceType: PieceType,
  position: VecI,
  owner: Int = -1,
  hasMoved: Boolean = false,
  id: Int = (new Random).nextInt(),
) extends PartialPiece.WithPosition, PartialPiece.WithOwner:
  export pieceType.{actions as _, *}
  def actions: Rule = Rule(pieceType.actions(this))
  
  def ownedBy(player: Int): Boolean = owner == player
  def byOwner[X](x: X*): X = x(owner)
  def is(pieceType: Any): Boolean = pieceType == pieceType
  def is[Q](using C: ClassTag[Q]): Boolean =
    pieceType.getClass == C.runtimeClass
    
  def move(to: VecI ?=> Kernel[?]): Rule = Generator.move(this.position -> to(using position))
  def replace(piece: PieceType*): Rule = Generator.place(piece -> this.position)
  
  def relocate(to: VecI)(using pieces: PieceSet)(using Kernel[?]): PieceSet =
    pieces.relocate(Kernel(this.position) -> Kernel(to))
  def remove(using pieces: PieceSet): PieceSet =
    pieces.remove(position)
    
  def texture: Texture = pieceType.texture(this)
  
object Piece:
  
  trait PieceType:
    def actions(piece: Piece): GameState ?=> Rule
    def texture(piece: Piece): boards.graphics.Texture =
      textures(piece.owner % textures.size)
    def textures: Seq[boards.graphics.Texture] = Seq()
    
  object PieceType:
    trait WithTexture(override val textures: Texture*) extends PieceType
    trait WithRule(val rule: Piece => GameState ?=> Rule) extends PieceType:
      def actions(piece: Piece): GameState ?=> Rule = rule(piece)
    abstract class Immobile extends WithRule(_ => Rule.none)
      
  trait PartialPiece:
    def pieceType: PieceType
    
  object PartialPiece:
    trait WithPosition extends PartialPiece:
      def position: VecI
    trait WithOwner extends PartialPiece:
      def owner: Int