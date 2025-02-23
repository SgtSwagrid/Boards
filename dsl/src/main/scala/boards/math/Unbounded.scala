package boards.math

import boards.graphics.Texture.O
import boards.math.Algebra.{*, given}

/** Extends an existing type to also include positive and negative infinity.
  * @tparam X The base type.
  */
enum Unbounded [@specialized +X: OrderedRing]:
  
  /** Any value from the base type. */
  case Finite [+Y: OrderedRing] (x: Y) extends Unbounded[Y]
  /** The unique value which is greater than any other value. */
  case PositiveInfinity extends Unbounded[Nothing]
  /** The unique value which is less than any other value. */
  case NegativeInfinity extends Unbounded[Nothing]
  
  /** Whether this value is finite (anything other than positive or negative infinity). */
  def isFinite: Boolean = this match
    case Finite(_) => true
    case _ => false
  
  /** Whether this value is equal to positive or negative infinity. */
  def isInfinite: Boolean = !isFinite
  
  /** Assume this value is finite, throwing an exception if it isn't. */
  def toBounded: X = this match
    case Finite(x) => x
    case _ => throw new IllegalStateException("Value is infinite.")
    
  def map [Y: OrderedRing] (f: X => Y): Unbounded[Y] = this match
    case Finite(x) => Finite(f(x))
    case PositiveInfinity => PositiveInfinity
    case NegativeInfinity => NegativeInfinity
    
  override def toString = this match
    case Finite(x) => x.toString
    case PositiveInfinity => "∞"
    case NegativeInfinity => "-∞"

object Unbounded:
  
  /** An integer which can also take the values positive or negative infinity. */
  type UInt = Unbounded[Int]
  /** A long which can also take the values positive or negative infinity. */
  type ULong = Unbounded[Long]
  /** A float which can also take the values positive or negative infinity. */
  type UFloat = Unbounded[Float]
  /** A double which can also take the values positive or negative infinity. */
  type UDouble = Unbounded[Double]
  /** A rational which can also take the values positive or negative infinity. */
  type URational = Unbounded[Rational]
  /** A surd which can also take the values positive or negative infinity. */
  type USurd = Unbounded[Surd]
  
  given [X: OrderedRing as R]: OrderedRing[Unbounded[X]] with
    
    inline def sum(x: Unbounded[X], y: Unbounded[X]): Unbounded[X] = (x, y) match
      // Finite values add as before.
      case (Finite(x), Finite(y)) => Finite(x + y)
      // Assumption: positive and negative infinity cancel out when added.
      case (NegativeInfinity, PositiveInfinity) | (NegativeInfinity, PositiveInfinity) =>
        Finite(R.additiveIdentity)
      case (NegativeInfinity, _) | (_, NegativeInfinity) => NegativeInfinity
      case (PositiveInfinity, _) | (_, PositiveInfinity) => PositiveInfinity
      
    inline def additiveIdentity: Unbounded[X] = Finite(R.additiveIdentity)
    
    inline def additiveInverse(x: Unbounded[X]): Unbounded[X] = x match
      // Finite values negate as before.
      case Finite(x) => Finite(-x)
      // Positive and negative infinity are the negations of each other.
      case PositiveInfinity => NegativeInfinity
      case NegativeInfinity => PositiveInfinity
    
    inline def product(x: Unbounded[X], y: Unbounded[X]): Unbounded[X] = (x, y) match
      // Finite values multiply as before.
      case (Finite(x), Finite(y)) => Finite(x * y)
      case (Finite(Zero()), _) | (_, Finite(Zero())) => Finite(R.additiveIdentity)
      case (PositiveInfinity, PositiveInfinity) | (NegativeInfinity, NegativeInfinity) => PositiveInfinity
      case (NegativeInfinity, PositiveInfinity) | (NegativeInfinity, PositiveInfinity) => NegativeInfinity
      case (Finite(Positive()), PositiveInfinity) | (PositiveInfinity, Finite(Positive())) => PositiveInfinity
      case (Finite(Negative()), NegativeInfinity) | (NegativeInfinity, Finite(Negative())) => PositiveInfinity
      case (Finite(Negative()), PositiveInfinity) | (PositiveInfinity, Finite(Negative())) => NegativeInfinity
      case (Finite(Positive()), NegativeInfinity) | (NegativeInfinity, Finite(Positive())) => NegativeInfinity
      case _ => throw new IllegalStateException
      
    inline def multiplicativeIdentity: Unbounded[X] = Finite(R.multiplicativeIdentity)
    
    inline def compare(x: Unbounded[X], y: Unbounded[X]) = (x, y) match
      // Finite values are ordered as before.
      case (Finite(x), Finite(y)) => R.compare(x, y)
      // Negative infinity is before everything else, positive infinity is after everything else.
      case (NegativeInfinity, NegativeInfinity) | (PositiveInfinity, PositiveInfinity) => 0
      case (NegativeInfinity, _) | (_, PositiveInfinity) => -1
      case (PositiveInfinity, _) | (_, NegativeInfinity) => 1