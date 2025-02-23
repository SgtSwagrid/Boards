package boards.math

import boards.math.Bijection.<=>
import boards.math.region.Vec
import boards.math.region.Vec.UVec
import boards.math.Algebra.{*, given}
import boards.math.Conversions.{*, given}
import boards.math.Unbounded.Finite

trait Bijection [A, B]:
  
  def apply (a: A): B
  def inverse (b: B): A
  
  def invert: Bijection[B, A] =
    val self = this
    new Bijection:
      def apply (b: B): A = self.inverse(b)
      def inverse (a: A): B = self.apply(a)
  
  def andThen [C] (g: B <=> C): A <=> C =
    val f = this
    new Bijection:
      def apply (a: A): C = g(f(a))
      def inverse (c: C): A = f.inverse(g.inverse(c))

object Bijection:
  
  infix type <=> [A, B] = Bijection[A, B]
  
  def apply [A, B] (
    apply: A => B,
    inverse: B => A,
  ): A <=> B = GenericBijection(apply, inverse)
  
  private case class GenericBijection[A, B] (
    f: A => B,
    g: B => A,
  ) extends Bijection[A, B]:
    def apply (a: A): B = f(a)
    def inverse (b: B): A = g(b)
  
  trait AffineBijection [X: OrderedRing, Y: OrderedRing]:
    
    def finite: Vec[X] <=> Vec[Y]
    def infinite: UVec[X] <=> UVec[Y]
  
  class Flip [X: OrderedRing] (
    axis: Int,
  ) extends AffineBijection[X, X]:
    
    val finite = new Bijection:
      def apply (v: Vec[X]) = v.flip(axis)
      def inverse (v: Vec[X]) = v.flip(axis)
    
    val infinite = new Bijection:
      def apply (v: UVec[X]) = v.flip(axis)
      def inverse (v: UVec[X]) = v.flip(axis)
  
  class Rotate [X: OrderedRing] (
    from: Int,
    to: Int,
  ) extends AffineBijection[X, X]:
    
    val finite = new Bijection:
      def apply (v: Vec[X]) = v.rotate(from, to)
      def inverse (v: Vec[X]) = v.rotate(to, from)
    
    val infinite = new Bijection:
      def apply (v: UVec[X]) = v.rotate(from, to)
      def inverse (v: UVec[X]) = v.rotate(to, from)
  
  class Translate [X: OrderedRing] (
    offset: Vec[X],
  ) extends AffineBijection[X, X]:
    
    val finite = new Bijection:
      def apply (v: Vec[X]) = v + offset
      def inverse (v: Vec[X]) = v - offset
    
    val infinite = new Bijection:
      def apply (v: UVec[X]) = v + offset.toUnbounded
      def inverse (v: UVec[X]) = v - offset.toUnbounded
  
  class Scale [X: OrderedField] (
    factor: X,
  ) extends AffineBijection[X, X]:
    
    val finite = new Bijection:
      def apply (v: Vec[X]) = v * factor
      def inverse (v: Vec[X]) = v * factor.inverse
    
    val infinite = new Bijection:
      def apply (v: UVec[X]) = v * Finite(factor)
      def inverse (v: UVec[X]) = v * Finite(factor.inverse)