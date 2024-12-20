package boards.math

import boards.math.Algebra.{*, given}
import boards.math.WithInfinity.{*, given}
import boards.math.Bijection.*
import boards.math.region.Vec
import boards.math.region.Vec.UVec

trait Bijection[A, B]:
  
  def apply(a: A): B
  def inverse(b: B): A
  
  def invert: Bijection[B, A] =
    val self = this
    new Bijection:
      def apply(b: B): A = self.inverse(b)
      def inverse(a: A): B = self.apply(a)
  
  def andThen[C](g: B <=> C): A <=> C =
    val f = this
    new Bijection:
      def apply(a: A): C = g(f(a))
      def inverse(c: C): A = f.inverse(g.inverse(c))

object Bijection:
  infix type <=> [A, B] = Bijection[A, B]
  
  trait AffineBijection[X: Ring: Ordering, Y: Ring: Ordering]:
    
    def finite: Vec[X] <=> Vec[Y]
    def infinite: UVec[X] <=> UVec[Y]
  
  class Flip [X: Ring: Ordering] (
    axis: Int,
  ) extends AffineBijection[X, X]:
    
    val finite = new Bijection:
      def apply(v: Vec[X]) = v.flip(axis)
      def inverse(v: Vec[X]) = v.flip(axis)
    
    val infinite = new Bijection:
      def apply(v: UVec[X]) = v.flip(axis)
      def inverse(v: UVec[X]) = v.flip(axis)
  
  class Rotate [X: Ring: Ordering] (
    from: Int,
    to: Int,
  ) extends AffineBijection[X, X]:
    
    val finite = new Bijection:
      def apply(v: Vec[X]) = v.rotate(from, to)
      def inverse(v: Vec[X]) = v.rotate(to, from)
    
    val infinite = new Bijection:
      def apply(v: UVec[X]) = v.rotate(from, to)
      def inverse(v: UVec[X]) = v.rotate(to, from)
  
  class Translate [X: Ring: Ordering] (
    offset: Vec[X],
  ) extends AffineBijection[X, X]:
    
    val finite = new Bijection:
      def apply(v: Vec[X]) = v + offset
      def inverse(v: Vec[X]) = v - offset
    
    val infinite = new Bijection:
      def apply(v: UVec[X]) = v + offset.asUnbounded
      def inverse(v: UVec[X]) = v - offset.asUnbounded
  
  class Scale [X: Field: Ordering] (
    factor: X,
  ) extends AffineBijection[X, X]:
    
    val finite = new Bijection:
      def apply(v: Vec[X]) = v * factor
      def inverse(v: Vec[X]) = v * factor.inverse
    
    val infinite = new Bijection:
      def apply(v: UVec[X]) = v * Finite(factor)
      def inverse(v: UVec[X]) = v * Finite(factor.inverse)