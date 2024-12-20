package boards.math

import boards.math.Algebra.{*, given}
import boards.math.WithInfinity.{*, given}
import boards.math.Interval.*
import boards.math.region.{BoundingBox, Vec}

import scala.reflect.ClassTag

enum Interval[@specialized X: Ring as R: Ordering as O]:
  case NonEmpty [Y: Ring: Ordering] (start: Y, end: Y) extends Interval[Y]
  case Empty [Y: Ring: Ordering] () extends Interval[Y]
  
  def | (that: Interval[X]): Interval[X] = (this, that) match
    case (NonEmpty(s1, e1), NonEmpty(s2, e2)) =>
      Interval.apply(s1 min s2, e1 max e2)
    case (left, Empty()) => left
    case (Empty(), right) => right
  
  def & (that: Interval[X]): Interval[X] = (this, that) match
    case (NonEmpty(s1, e1), NonEmpty(s2, e2)) =>
      Interval.apply(s1 max s2, e1 min e2)
    case (Empty(), _) | (_, Empty()) => Empty()
    
  def contains(x: X): Boolean = this match
    case NonEmpty(start, end) => start <= x && x <= end
    case Empty() => false
    
  def contains[Y](using X =:= WithInfinity[Y])(y: Y): Boolean =
    contains(Finite(y).asInstanceOf[X])
    
  def length: X = this match
    case NonEmpty(start, end) => end - start + R.multiplicativeIdentity
    case Empty() => R.additiveIdentity
  
  def shift(x: X): Interval[X] = this match
    case NonEmpty(start, end) => NonEmpty(start + x, end + x)
    case Empty() => Empty()
  
  def shift[Y](using X =:= WithInfinity[Y])(y: Y): Interval[X] =
    shift(Finite(y).asInstanceOf[X])
  
  def shiftStart(x: X): Interval[X] = this match
    case NonEmpty(start, end) => Interval(start + x, end)
    case Empty() => Empty()
    
  def shiftStart[Y](using X =:= WithInfinity[Y])(y: Y): Interval[X] =
    shiftStart(Finite(y).asInstanceOf[X])
  
  def shiftEnd(x: X): Interval[X] = this match
    case NonEmpty(start, end) => Interval(start, end + x)
    case Empty() => Empty()
  
  def shiftEnd[Y](using X =:= WithInfinity[Y])(y: Y): Interval[X] =
    shiftEnd(Finite(y).asInstanceOf[X])
    
  def before(x: X): Interval[X] = this match
    case NonEmpty(start, end) => Interval(start min x, end min x)
    case Empty() => Empty()
    
  def before[Y](using X =:= WithInfinity[Y])(y: Y): Interval[X] =
    before(Finite(y).asInstanceOf[X])
  
  def after(x: X): Interval[X] = this match
    case NonEmpty(start, end) => Interval(start max x, end max x)
    case Empty() => Empty()
  
  def after[Y](using X =:= WithInfinity[Y])(y: Y): Interval[X] =
    after(Finite(y).asInstanceOf[X])
    
  def orElse(x: X): NonEmpty[X] = this match
    case NonEmpty(start, end) => NonEmpty(start, end)
    case Empty() => NonEmpty(x, x)
    
  def orElse[Y](using X =:= WithInfinity[Y])(y: Y): Interval[X] =
    orElse(Finite(y).asInstanceOf[X])
  
  def asUnbounded: UInterval[X] = this match
    case NonEmpty(start, end) => NonEmpty(Finite(start), Finite(end))
    case Empty() => Empty()
  
  def isEmpty: Boolean = this match
    case NonEmpty(_, _) => false
    case Empty() => true
    
  def nonEmpty: Boolean = !isEmpty
  
  def isFinite(using X <:< WithInfinity[?]): Boolean = this match
    case NonEmpty(start, end) => start.isFinite && end.isFinite
    case Empty() => true
  
  def isInfinite(using X <:< WithInfinity[?]): Boolean = !isFinite
  
  def asFinite[Y: Ring: Ordering](using X <:< WithInfinity[Y]): Interval[Y] = this match
    case NonEmpty(start, end) => Interval(start.asFinite, end.asFinite)
    case Empty() => Empty()
    
  override def toString = this match
    case NonEmpty(start, end) => s"($start, $end)"
    case Empty() => "∅"

object Interval:
  
  type IntervalI = Interval[Int]
  type IntervalL = Interval[Long]
  type IntervalF = Interval[Float]
  type IntervalD = Interval[Double]
  
  type UInterval[X] = Interval[WithInfinity[X]]
  type UIntervalI = Interval[ExtendedInt]
  type UIntervalL = Interval[ExtendedLong]
  type UIntervalF = Interval[ExtendedFloat]
  type UIntervalD = Interval[ExtendedDouble]
    
  def apply [X: Ring: Ordering] (start: X, end: X): Interval[X] =
    if start <= end
    then NonEmpty(start, end)
    else Empty()
    
  def bounding [X: Ring: Ordering as O] (vs: X*): Interval[X] =
    Interval(vs.min, vs.max)
  
  def none[X: Ring: Ordering]: Interval[X] = Interval.Empty()
  
  object UInterval:
    
    def apply[X: Ring: Ordering](start: X, end: X): UInterval[X] =
      if start < end
      then NonEmpty(Finite(start), Finite(end))
      else Empty()
  
    def from[X: Ring: Ordering](start: X): UInterval[X] =
      Interval(Finite(start), PositiveInfinity)
      
    def to[X: Ring: Ordering](end: X): UInterval[X] =
      Interval(NegativeInfinity, Finite(end))
      
    def all[X: Ring: Ordering]: UInterval[X] = Interval(NegativeInfinity, PositiveInfinity)
  
  given [X: Ring as R: Ordering]: Group[Interval[X]] with
    
    def sum(x: Interval[X], y: Interval[X]): Interval[X] = (x, y) match
      case (NonEmpty(s1, e1), NonEmpty(s2, e2)) => NonEmpty(s1 + s2, e1 + e2)
      case _ => Empty()
    
    def additiveIdentity: Interval[X] =
      Interval(R.additiveIdentity, R.additiveIdentity)
    
    def additiveInverse(x: Interval[X]): Interval[X] = x match
      case NonEmpty(start, end) => NonEmpty(-end, -start)
      case Empty() => Empty()