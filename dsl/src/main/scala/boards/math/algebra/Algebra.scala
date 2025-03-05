package boards.math.algebra

import boards.math.vector.{Region, Vec}

import scala.annotation.targetName

object Algebra:
  
  trait Identity [+X]:
    def identity: X
  
  trait Semigroup [X]:
    def sum (x: X, y: X): X
    
  extension [X: Semigroup as S] (x: X)
    def + (y: X): X = S.sum(x, y)
  
  trait Monoid [X] extends Semigroup[X]:
    def additiveIdentity: X
    def zero: X = additiveIdentity
    
  extension [X: Monoid as M] (x: X)
    def isZero: Boolean = x == M.additiveIdentity
    
  extension [X: {Monoid as M, Ordering as O}] (x: X)
    def isPositive: Boolean = x > M.additiveIdentity
    def isNonNegative: Boolean = x >= M.additiveIdentity
    def isNegative: Boolean = x < M.additiveIdentity
    def isNonPositive: Boolean = x <= M.additiveIdentity
  
  object Zero:
    def unapply [X: Monoid] (x: X): Boolean = x.isZero
  
  object Positive:
    def unapply [X: {Monoid, Ordering}] (x: X): Boolean = x.isPositive
      
  object Negative:
    def unapply [X: {Monoid, Ordering}] (x: X): Boolean = x.isNegative
    
  trait Group [X] extends Monoid[X]:
    def additiveInverse (x: X): X
    def sign (cond: Boolean): X =
      if cond then additiveIdentity else additiveInverse(additiveIdentity)
      
  extension [X: Group as G] (x: X)
    def unary_- : X = G.additiveInverse(x)
    def - (y: X): X = x + -y
      
  extension [X: {Group as G, Ordering as O}] (x: X)
    def sign: X = G.sign(x >= G.additiveIdentity)
  
  trait Ring [X] extends Group[X]:
    def product (x: X, y: X): X
    def multiplicativeIdentity: X
    def one: X = multiplicativeIdentity
    def two: X = sum(one, one)
    
  extension [X: Ring as R] (x: X)
    def * (y: X): X = R.product(x, y)
    def * (v: Vec[X]): Vec[X] = v * x
    
  trait Dividable [X]:
    def divide (x: X, y: X): X
    
  extension [X: Dividable as D] (x: X)
    def / (y: X): X = D.divide(x, y)
    
  trait DividableRing [X] extends Ring[X], Dividable[X]
    
  trait Field [X] extends DividableRing[X]:
    def multiplicativeInverse (x: X): X
    def divide (x: X, y: X): X = product(x, multiplicativeInverse(y))
    
  extension [X: Field as F] (x: X)
    def inverse: X = F.multiplicativeInverse(x)
    
  extension [X: OrderedField] (x: X)
    def * (region: Region[X]): Region[X] = region.scale(x)
  
  trait OrderedSemigroup[X] extends Semigroup[X], Ordering[X]
  trait OrderedMonoid[X] extends Monoid[X], Ordering[X], OrderedSemigroup[X]
  trait OrderedGroup[X] extends Group[X], Ordering[X], OrderedMonoid[X]
  trait OrderedRing[X] extends Ring[X], Ordering[X], OrderedGroup[X]
  trait Numeric[X] extends Dividable[X], Ordering[X], OrderedRing[X]
  trait OrderedField[X] extends Field[X], Ordering[X], Numeric[X]
    
  extension [X: Ordering as O] (x: X)
    def < (y: X): Boolean = O.lt(x, y)
    def < (v: Vec[X]): Boolean = v > x
    def <= (y: X): Boolean = O.lteq(x, y)
    def <= (v: Vec[X]): Boolean = v >= x
    def > (y: X): Boolean = O.gt(x, y)
    def > (v: Vec[X]): Boolean = v < x
    def >= (y: X): Boolean = O.gteq(x, y)
    def >= (v: Vec[X]): Boolean = v <= x
    infix def min (y: X): X = if x <= y then x else y
    infix def max (y: X): X = if x >= y then x else y
    
  given Identity[Unit] with
    inline def identity: Unit = ()
    
  given [X, Y] (using x: Identity[X], y: Identity[Y]): Identity[(X, Y)] with
    inline def identity: (X, Y) = (x.identity, y.identity)
    
  given Numeric[Int] with
    inline def sum (x: Int, y: Int): Int = x + y
    inline def product (x: Int, y: Int): Int = x * y
    inline def additiveIdentity: Int = 0
    inline def multiplicativeIdentity: Int = 1
    inline def additiveInverse (x: Int): Int = -x
    inline def compare (x: Int, y: Int): Int = x - y
    inline def divide(x: Int, y: Int): Int = x / y
    
  given Numeric[Long] with
    inline def sum (x: Long, y: Long): Long = x + y
    inline def product (x: Long, y: Long): Long = x * y
    inline def additiveIdentity: Long = 0
    inline def multiplicativeIdentity: Long = 1
    inline def additiveInverse (x: Long): Long = -x
    inline def compare (x: Long, y: Long): Int = java.lang.Long.compare(x, y)
    inline def divide (x: Long, y: Long): Long = x / y
  
  given Numeric[Boolean] with
    inline def sum (x: Boolean, y: Boolean): Boolean = x | y
    inline def product (x: Boolean, y: Boolean): Boolean = x & y
    inline def additiveIdentity: Boolean = false
    inline def multiplicativeIdentity: Boolean = true
    inline def additiveInverse (x: Boolean): Boolean = !x
    inline def compare (x: Boolean, y: Boolean): Int = (x, y) match
      case (true, true) | (false, false) => 0
      case (false, true) => -1
      case (true, false) => 1
    inline def divide (x: Boolean, y: Boolean) = x & y
    
  given OrderedField[Float] with
    inline def sum (x: Float, y: Float): Float = x + y
    inline def product (x: Float, y: Float): Float = x * y
    inline def additiveIdentity: Float = 0
    inline def multiplicativeIdentity: Float = 1
    inline def additiveInverse (x: Float): Float = -x
    inline def multiplicativeInverse(x: Float): Float = 1.0F / x
    inline def compare(x: Float, y: Float): Int = java.lang.Float.compare(x, y)
  
  given OrderedField[Double] with
    inline def sum (x: Double, y: Double): Double = x + y
    inline def product (x: Double, y: Double): Double = x * y
    inline def additiveIdentity: Double = 0
    inline def multiplicativeIdentity: Double = 1
    inline def additiveInverse (x: Double): Double = -x
    inline def multiplicativeInverse (x: Double): Double = 1.0D / x
    inline def compare (x: Double, y: Double): Int = java.lang.Double.compare(x, y)
    
  given OrderedField[Nothing] with
    inline def sum (x: Nothing, y: Nothing): Nothing = throw new IllegalArgumentException
    inline def product (x: Nothing, y: Nothing): Nothing = throw new IllegalArgumentException
    inline def additiveIdentity: Nothing = throw new IllegalArgumentException
    inline def multiplicativeIdentity: Nothing = throw new IllegalArgumentException
    inline def additiveInverse (x: Nothing): Nothing = throw new IllegalArgumentException
    inline def multiplicativeInverse (x: Nothing): Nothing = throw new IllegalArgumentException
    inline def compare (x: Nothing, y: Nothing): Nothing = throw new IllegalArgumentException