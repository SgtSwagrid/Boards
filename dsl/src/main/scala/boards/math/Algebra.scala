package boards.math

import boards.math.region.{Region, Vec}

import scala.annotation.targetName

object Algebra:
  
  trait Identity[+X]:
    def identity: X
  
  trait Semigroup[X]:
    def sum(x: X, y: X): X
    
  extension [X: Semigroup as S] (x: X)
    def + (y: X): X = S.sum(x, y)
  
  trait Monoid[X] extends Semigroup[X]:
    def additiveIdentity: X
    
  extension [X: Monoid as M] (x: X)
    def isZero: Boolean = x == M.additiveIdentity
    
  extension [X: Monoid as M: Ordering as O] (x: X)
    def isPositive: Boolean = x > M.additiveIdentity
    def isNonNegative: Boolean = x >= M.additiveIdentity
    def isNegative: Boolean = x < M.additiveIdentity
    def isNonPositive: Boolean = x <= M.additiveIdentity
  
  object Zero:
    def unapply[X: Monoid](x: X): Boolean = x.isZero
  
  object Positive:
    def unapply[X: Monoid: Ordering](x: X): Boolean = x.isPositive
      
  object Negative:
    def unapply[X: Monoid: Ordering](x: X): Boolean = x.isNegative
    
  trait Group[X] extends Monoid[X]:
    def additiveInverse(x: X): X
    def sign(cond: Boolean): X =
      if cond then additiveIdentity else additiveInverse(additiveIdentity)
      
  extension [X: Group as G] (x: X)
    def unary_- : X = G.additiveInverse(x)
    def - (y: X): X = x + -y
      
  extension [X: Group as G: Ordering as O] (x: X)
    def sign: X = G.sign(x >= G.additiveIdentity)
  
  trait Ring[X] extends Group[X]:
    def product (x: X, y: X): X
    def multiplicativeIdentity: X
    
  extension [X: Ring as R] (x: X)
    def * (y: X): X = R.product(x, y)
    def * (v: Vec[X]): Vec[X] = v * x
    
  trait Field[X] extends Ring[X]:
    def multiplicativeInverse(x: X): X
    
  extension [X: Field as F] (x: X)
    def inverse: X = F.multiplicativeInverse(x)
    def / (y: X): X = x * y.inverse
    //def * (region: Region[X]): Region[X] = region * x
    
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
    
  given [X, Y](using x: Identity[X], y: Identity[Y]): Identity[(X, Y)] with
    inline def identity: (X, Y) = (x.identity, y.identity)
    
  given Ring[Int] with
    inline def sum(x: Int, y: Int): Int = x + y
    inline def product(x: Int, y: Int): Int = x * y
    inline def additiveIdentity: Int = 0
    inline def multiplicativeIdentity: Int = 1
    inline def additiveInverse(x: Int): Int = -x
    
  given Ring[Long] with
    inline def sum(x: Long, y: Long): Long = x + y
    inline def product(x: Long, y: Long): Long = x * y
    inline def additiveIdentity: Long = 0
    inline def multiplicativeIdentity: Long = 1
    inline def additiveInverse(x: Long): Long = -x
  
  given Ring[Boolean] with
    inline def sum(x: Boolean, y: Boolean): Boolean = x | y
    inline def product(x: Boolean, y: Boolean): Boolean = x & y
    inline def additiveIdentity: Boolean = false
    inline def multiplicativeIdentity: Boolean = true
    inline def additiveInverse(x: Boolean): Boolean = !x
    
  given Field[Float] with
    inline def sum(x: Float, y: Float): Float = x + y
    inline def product(x: Float, y: Float): Float = x * y
    inline def additiveIdentity: Float = 0
    inline def multiplicativeIdentity: Float = 1
    inline def additiveInverse(x: Float): Float = -x
    inline def multiplicativeInverse(x: Float): Float = 1.0F / x
  
  given Field[Double] with
    inline def sum(x: Double, y: Double): Double = x + y
    inline def product(x: Double, y: Double): Double = x * y
    inline def additiveIdentity: Double = 0
    inline def multiplicativeIdentity: Double = 1
    inline def additiveInverse(x: Double): Double = -x
    inline def multiplicativeInverse(x: Double): Double = 1.0D / x
    
  given Field[Nothing] with
    inline def sum(x: Nothing, y: Nothing): Nothing = throw new IllegalArgumentException
    inline def product(x: Nothing, y: Nothing): Nothing = throw new IllegalArgumentException
    inline def additiveIdentity: Nothing = throw new IllegalArgumentException
    inline def multiplicativeIdentity: Nothing = throw new IllegalArgumentException
    inline def additiveInverse(x: Nothing): Nothing = throw new IllegalArgumentException
    inline def multiplicativeInverse(x: Nothing): Nothing = throw new IllegalArgumentException