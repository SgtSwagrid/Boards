package boards.math

import scala.annotation.targetName

object Algebra:
  
  trait Identity[+T]:
    def identity: T
  
  trait Semigroup[T]:
    def sum(x: T, y: T): T
  
  trait Monoid[T] extends Semigroup[T]:
    def additiveIdentity: T
    
  trait Group[T] extends Monoid[T]:
    def additiveInverse(x: T): T
    def sign(cond: Boolean): T =
      if cond then additiveIdentity else additiveInverse(additiveIdentity)
    
  trait Ring[T] extends Group[T]:
    def product (x: T, y: T): T
    def multiplicativeIdentity: T
    
  trait Field[T] extends Ring[T]:
    def multiplicativeInverse(x: T): T
    
  extension [X] (x: X) (using S: Semigroup[X])
    def + (y: X): X = S.sum(x, y)
    
  extension [X] (x: X) (using G: Group[X])
    def unary_- : X = G.additiveInverse(x)
    def - (y: X): X = x + -y
    
  extension [X] (x: X) (using R: Ring[X])
    def * (y: X): X = R.product(x, y)
    def * (v: Vec[X]): Vec[X] = v * x
    def * [T] (region: Region[X, T]): Region[X, T] = region * x
    
  extension [X] (x: X) (using R: Ring[X], O: Ordering[X])
    def < (y: X): Boolean = O.lt(x, y)
    def < (v: Vec[X]): Boolean = v > x
    def <= (y: X): Boolean = O.lteq(x, y)
    def <= (v: Vec[X]): Boolean = v >= x
    def > (y: X): Boolean = O.gt(x, y)
    def > (v: Vec[X]): Boolean = v < x
    def >= (y: X): Boolean = O.gteq(x, y)
    def >= (v: Vec[X]): Boolean = v <= x
    
  extension [X] (x: X) (using F: Field[X])
    def inverse: X = F.multiplicativeInverse(x)
    def / (y: X): X = x * y.inverse
    
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