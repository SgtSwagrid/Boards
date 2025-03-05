package boards.math.algebra

import Algebra.{*, given}
import Bijection.*
import boards.math.vector.Vec
import boards.math.vector.Vec.{UVec, given}
import boards.math.algebra.Unbounded.Finite
import io.circe.Codec

trait Bijection [A, B] extends Function[A, B]:
  
  def apply (a: A): B
  def inverse (b: B): A
  
  final def compose [C] (that: C <=> A): C <=> B =
    that.andThen(this)
  
  final def andThen [C] (f2: B <=> C): A <=> C =
    val f1 = this
    new Bijection:
      def apply (a: A): C = f2(f1(a))
      def inverse (c: C): A = f1.inverse(f2.inverse(c))
    
  def invert: B <=> A =
    val f = this
    new Bijection:
      def apply (b: B): A = f.inverse(b)
      def inverse (a: A): B = f(a)
    
object Bijection:

  infix type <=> [A, B] = Bijection[A, B]
  
  sealed trait AffineBijection [X, Y] extends Bijection[UVec[X], UVec[Y]]//:
    //def invert: AffineBijection[Y, X]
  
  case class Translate [X] (offset: Vec[X]) extends AffineBijection[X, X]:
    def apply (v: UVec[X]): UVec[X] = v.translate(offset.toUnbounded)
    def inverse (v: UVec[X]): UVec[X] = v.translate(-offset.toUnbounded)
    override def invert: Translate[X] = Translate(-offset)
  
  case class Flip [X] (axis: Int) extends AffineBijection[X, X]:
    def apply (v: UVec[X]): UVec[X] = v.flip(axis)
    def inverse (v: UVec[X]): UVec[X] = v.flip(axis)
    override def invert: Flip[X] = this
  
  case class Rotate [X] (from: Int, to: Int) extends AffineBijection[X, X]:
    def apply (v: UVec[X]): UVec[X] = v.rotate(from, to)
    def inverse (v: UVec[X]): UVec[X] = v.rotate(to, from)
    override def invert: Rotate[X] = Rotate(to, from)
  
  case class Scale [X] (factors: Vec[X]) extends AffineBijection[X, X]:
    def this (factor: X) (using Numeric[X]) = this(Vec.fillForever(factor))
    def apply (v: UVec[X]): UVec[X] = v * factors.toUnbounded
    def inverse (v: UVec[X]): UVec[X] = v / factors.toUnbounded
    
  given [X: {Numeric, Codec}]: Codec[AffineBijection[X, X]] =
    Codec.derived