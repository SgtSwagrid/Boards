package boards.algebra

import boards.algebra.GameState.{*, given}
import Piece.{*, given}
import boards.graphics.Texture
import util.math.kernel.{Dir, Kernel}
import util.extensions.Conversions.given
import util.math.Vec.VecI

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
    pieces.relocate(this -> to)
  def remove(using pieces: PieceSet): PieceSet =
    pieces.remove(position)
    
  def texture: Texture = pieceType.texture(this)
  
object Piece:
  
  trait PieceType:
    def actions(piece: Piece): GameState ?=> Rule
    def texture(piece: Piece): Texture =
      textures(piece.owner % textures.size)
    def textures: Seq[Texture] = Seq()
    
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
  
  given Conversion[Piece, VecI] with
    def apply(piece: Piece): VecI = piece.position
  
  given Conversion[Piece, Kernel.Shape] with
    def apply(piece: Piece): Kernel.Shape = Kernel(piece.position)
    
  given Conversion[Piece, PieceType] with
    def apply(piece: Piece): PieceType = piece.pieceType
  
  given Conversion[Piece, Rule] with
    def apply(piece: Piece): Rule = piece.actions