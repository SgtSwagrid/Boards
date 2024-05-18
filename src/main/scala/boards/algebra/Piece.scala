package boards.algebra

import util.math.Pos.{*, given}
import boards.algebra.GameState.{*, given}
import Piece.*
import util.math.kernel.{Dir, Kernel}
import util.extensions.Conversions.given

import scala.annotation.targetName
import scala.reflect.ClassTag

case class Piece (
  state: PieceType,
  position: Pos,
  owner: Int = -1,
  hasMoved: Boolean = false
):
  export state.{actions as _, *}
  def actions: Rule = Rule(state.actions(this))
  
  def ownedBy(player: Int): Boolean = owner == player
  def byOwner[X](x: X*): X = x(owner)
  def is(pieceType: Any): Boolean = state == pieceType
  def is[Q](using C: ClassTag[Q]): Boolean =
    state.getClass == C.runtimeClass
    
  def move(to: Pos ?=> Kernel[?]): Rule = Generator.move(this.position -> to(using position))
  def replace(piece: PieceType*): Rule = Generator.place(this.position -> piece)
  
object Piece:
  
  trait PieceType:
    def actions(piece: Piece): GameState ?=> Rule
    def texture(piece: Piece): String =
      textures(piece.owner % textures.size)
    def textures: Seq[String] = Seq("")
    
  object PieceType:
    trait WithTexture(override val textures: String*) extends PieceType
    trait Immobile extends PieceType:
      final def actions(piece: Piece) = Rule.none
  
  given Conversion[Piece, Pos] with
    def apply(piece: Piece): Pos = piece.position
  
  given Conversion[Piece, Kernel[?]] with
    def apply(piece: Piece): Kernel[?] = Kernel(piece.position)
  
  given Conversion[Piece, Rule] with
    def apply(piece: Piece): Rule = piece.actions