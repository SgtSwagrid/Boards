package util.math

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
    
  extension [X] (x: X) (using s: Semigroup[X])
    @targetName("add") def + (y: X): X = s.sum(x, y)
    
  extension [X] (x: X) (using g: Group[X])
    @targetName("negate") def unary_- : X = g.additiveInverse(x)
    @targetName("subtract") def - (y: X): X = x + -y
    
  extension [X] (x: X) (using r: Ring[X])
    @targetName("multiply") def * (y: X): X = r.product(x, y)
    @targetName("multiply") def * (v: Vec[X]): Vec[X] = v.map(x * _)
    @targetName("multiply") def * (m: Matrix[X]): Matrix[X] = m.map(x * _)
    
  extension [X] (x: X) (using f: Field[X])
    def inverse: X = f.multiplicativeInverse(x)
    @targetName("divide") def / (y: X): X = x * y.inverse
    
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