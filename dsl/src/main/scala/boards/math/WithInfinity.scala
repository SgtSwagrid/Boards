package boards.math

import boards.math.Algebra.{*, given}

enum WithInfinity[@specialized +X]:
  
  case Finite(x: X)
  case PositiveInfinity
  case NegativeInfinity
  
  def isFinite: Boolean = this match
    case Finite(_) => true
    case _ => false
  
  def isInfinite: Boolean = !isFinite
  
  def asFinite: X = this match
    case Finite(x) => x
    case _ => throw new IllegalStateException("Value is infinite.")
    
  override def toString = this match
    case Finite(x) => x.toString
    case PositiveInfinity => "∞"
    case NegativeInfinity => "-∞"

object WithInfinity:
  
  type ExtendedInt = WithInfinity[Int]
  type ExtendedLong = WithInfinity[Long]
  type ExtendedFloat = WithInfinity[Float]
  type ExtendedDouble = WithInfinity[Double]
  
  
  given [X: Identity as I]: Identity[WithInfinity[X]] with
    
    inline def identity: WithInfinity[X] = Finite(I.identity)
  
  
  given [X: Monoid]: Monoid[WithInfinity[X]] with InfinityMonoid[X]
  
  trait InfinityMonoid[X: Monoid as M] extends Monoid[WithInfinity[X]]:
    
    inline def sum(x: WithInfinity[X], y: WithInfinity[X]): WithInfinity[X] = (x, y) match
      case (Finite(x), Finite(y)) => Finite(x + y)
      case (NegativeInfinity, PositiveInfinity) | (NegativeInfinity, PositiveInfinity) =>
        Finite(M.additiveIdentity)
      case (NegativeInfinity, _) | (_, NegativeInfinity) => NegativeInfinity
      case (PositiveInfinity, _) | (_, PositiveInfinity) => PositiveInfinity
      
    inline def additiveIdentity: WithInfinity[X] = Finite(M.additiveIdentity)
    
    
  given [X: Group]: Group[WithInfinity[X]] with InfinityGroup[X]
  
  trait InfinityGroup[X: Group as G] extends Group[WithInfinity[X]], InfinityMonoid[X]:
    
    inline def additiveInverse(x: WithInfinity[X]): WithInfinity[X] = x match
      case Finite(x) => Finite(-x)
      case PositiveInfinity => NegativeInfinity
      case NegativeInfinity => PositiveInfinity
    
    
  given [X: Ring: Ordering]: Ring[WithInfinity[X]] with InfinityRing[X]
  
  trait InfinityRing[X: Ring as R: Ordering] extends Ring[WithInfinity[X]], InfinityGroup[X]:
    
    inline def product(x: WithInfinity[X], y: WithInfinity[X]): WithInfinity[X] = (x, y) match
      case (Finite(x), Finite(y)) => Finite(x * y)
      case (Finite(Zero()), _) | (_, Finite(Zero())) => Finite(R.additiveIdentity)
      case (PositiveInfinity, PositiveInfinity) | (NegativeInfinity, NegativeInfinity) => PositiveInfinity
      case (NegativeInfinity, PositiveInfinity) | (NegativeInfinity, PositiveInfinity) => NegativeInfinity
      case (Finite(Positive()), PositiveInfinity) | (PositiveInfinity, Finite(Positive())) => PositiveInfinity
      case (Finite(Negative()), NegativeInfinity) | (NegativeInfinity, Finite(Negative())) => PositiveInfinity
      case (Finite(Negative()), PositiveInfinity) | (PositiveInfinity, Finite(Negative())) => NegativeInfinity
      case (Finite(Positive()), NegativeInfinity) | (NegativeInfinity, Finite(Positive())) => NegativeInfinity
      case _ => throw new IllegalStateException
      
    inline def multiplicativeIdentity: WithInfinity[X] = Finite(R.multiplicativeIdentity)
  
  
  given [X: Ordering as O]: Ordering[WithInfinity[X]] with
    
    def compare(x: WithInfinity[X], y: WithInfinity[X]) = (x, y) match
      case (Finite(x), Finite(y)) => O.compare(x, y)
      case (NegativeInfinity, NegativeInfinity) | (PositiveInfinity, PositiveInfinity) => 0
      case (NegativeInfinity, _) | (_, PositiveInfinity) => -1
      case (PositiveInfinity, _) | (_, NegativeInfinity) => 1
  
  
  given [X]: Conversion[X, WithInfinity[X]] = x => Finite(x)