package boards.algebra.state

import boards.algebra.rules.Rule
import boards.algebra.rules.{Generator, Rule}
import boards.algebra.state.GameState
import boards.algebra.state.Piece.*
import boards.algebra.Game.PlayerId
import boards.imports.games.{*, given}
import boards.imports.math.{*, given}

import java.util.Random
import scala.annotation.targetName
import scala.reflect.ClassTag

case class Piece (
  pieceType: PieceType,
  position: VecI,
  owner: PlayerId = PlayerId(-1),
  hasMoved: Boolean = false,
  id: Int = (new Random).nextInt(),
) extends PartialPiece.WithPosition, PartialPiece.WithOwner:
  given PlayerId = owner
  export pieceType.{actions as _, *}
  def actions: Rule = Rule.switch(pieceType.actions(this)(using summon[GameState], owner))
  
  def ownedBy(player: PlayerId): Boolean = owner == player
  def byOwner[X](x: X*): X = x(owner.toInt)
  infix def is(pieceType: Any): Boolean = this.pieceType == pieceType
  def is[Q](using C: ClassTag[Q]): Boolean =
    pieceType.getClass == C.runtimeClass
    
  // add versions of these to PieceSet too!
    
  def move(to: VecI ?=> Ker): Rule =
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
  
object Piece:
  
  trait PieceType:
    def actions(piece: Piece): (GameState, PlayerId) ?=> Rule
    def texture(piece: Piece): boards.graphics.Texture =
      textures(piece.owner.toInt % textures.size)
    def textures: Seq[boards.graphics.Texture] = Seq()
    def hash: String = getClass.getSimpleName
    
    def place(owner: PlayerId)(pos: Ker*): Rule =
      Generator.place(owner)(this -> pos.reduce(_ | _))
    
    def place(pos: Ker*)(using PlayerId): Rule =
      Generator.place(this -> pos.reduce(_ | _))
      
    def insert(owner: PlayerId)(pos: Ker*): Rule =
      Effect.insert(owner)(this -> pos.reduce(_ | _))
      
    def insert(pos: Ker*)(using PlayerId): Rule =
      Effect.insert(this -> pos.reduce(_ | _))
    
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