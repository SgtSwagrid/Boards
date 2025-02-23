package boards.math

import boards.math.Algebra.{*, given}
import boards.math.Unbounded.{*, given}
import boards.math.Interval.*

/** Represents a set of scalar values in some contiguous interval.
  * @tparam X The discrete field over which the interval is defined.
  */
enum Interval [@specialized X: OrderedRing as R]:
  
  /** A strictly non-empty interval defined with a start and end such that start <= end. */
  case NonEmpty [Y: OrderedRing] (start: Y, end: Y) extends Interval[Y]
  /** An empty interval containing no points. */
  case Empty [Y: OrderedRing] () extends Interval[Y]
  
  /** Produces the minimal interval to contain both operands. */
  def | (that: Interval[X]): Interval[X] = (this, that) match
    case (NonEmpty(s1, e1), NonEmpty(s2, e2)) =>
      Interval.apply(s1 min s2, e1 max e2)
    case (left, Empty()) => left
    case (Empty(), right) => right
  
  /** Produces the maximal interval contained in both operands. */
  def & (that: Interval[X]): Interval[X] = (this, that) match
    case (NonEmpty(s1, e1), NonEmpty(s2, e2)) =>
      Interval.apply(s1 max s2, e1 min e2)
    case (Empty(), _) | (_, Empty()) => Empty()
    
  /** Determines whether the given point is contained within this interval. */
  def contains (x: X): Boolean = this match
    case NonEmpty(start, end) => start <= x && x <= end
    case Empty() => false
  
  /** Determines whether the given point is contained within this interval. */
  def contains [Y: OrderedRing] (using X =:= Unbounded[Y]) (y: Y): Boolean =
    contains(Finite(y).asInstanceOf[X])
    
  /** Determines the length of this interval. */
  def length: X = this match
    case NonEmpty(start, end) => end - start + R.multiplicativeIdentity
    case Empty() => R.additiveIdentity
  
  /** Shift the entire interval by the given offset. */
  def shift (x: X): Interval[X] = this match
    case NonEmpty(start, end) => NonEmpty(start + x, end + x)
    case Empty() => Empty()
  
  /** Shift the entire interval by the given offset. */
  def shift [Y: OrderedRing] (using X =:= Unbounded[Y]) (y: Y): Interval[X] =
    shift(Finite(y).asInstanceOf[X])
  
  /** Shift the start of the interval by the given offset. */
  def shiftStart(x: X): Interval[X] = this match
    case NonEmpty(start, end) => Interval(start + x, end)
    case Empty() => Empty()
  
  /** Shift the start of the interval by the given offset. */
  def shiftStart [Y: OrderedRing] (using X =:= Unbounded[Y]) (y: Y): Interval[X] =
    shiftStart(Finite(y).asInstanceOf[X])
  
  /** Shift the end of the interval by the given offset. */
  def shiftEnd (x: X): Interval[X] = this match
    case NonEmpty(start, end) => Interval(start, end + x)
    case Empty() => Empty()
  
  /** Shift the end of the interval by the given offset. */
  def shiftEnd [Y: OrderedRing] (using X =:= Unbounded[Y]) (y: Y): Interval[X] =
    shiftEnd(Finite(y).asInstanceOf[X])
  
  /** Take only the portion of the interval which lies before the given value. */
  def before (x: X): Interval[X] = this match
    case NonEmpty(start, end) => Interval(start min x, end min x)
    case Empty() => Empty()
  
  /** Take only the portion of the interval which lies before the given value. */
  def before [Y: OrderedRing] (using X =:= Unbounded[Y]) (y: Y): Interval[X] =
    before(Finite(y).asInstanceOf[X])
  
  /** Take only the portion of the interval which lies after the given value. */
  def after (x: X): Interval[X] = this match
    case NonEmpty(start, end) => Interval(start max x, end max x)
    case Empty() => Empty()
  
  /** Take only the portion of the interval which lies after the given value. */
  def after [Y: OrderedRing] (using X =:= Unbounded[Y]) (y: Y): Interval[X] =
    after(Finite(y).asInstanceOf[X])
  
  /** Provide an alternative value in case this interval is empty. */
  def orElse (x: X): NonEmpty[X] = this match
    case NonEmpty(start, end) => NonEmpty(start, end)
    case Empty() => NonEmpty(x, x)
  
  /** Provide an alternative value in case this interval is empty. */
  def orElse [Y: OrderedRing] (using X =:= Unbounded[Y]) (y: Y): Interval[X] =
    orElse(Finite(y).asInstanceOf[X])
  
  /** Lift this interval to an algebra with infinite values. */
  def toUnbounded: UInterval[X] = this match
    case NonEmpty(start, end) => NonEmpty(Finite(start), Finite(end))
    case Empty() => Empty()
  
  /** Whether this interval contains no values. */
  def isEmpty: Boolean = this match
    case NonEmpty(_, _) => false
    case Empty() => true
  
  /** Whether this interval contains at least one point. */
  def nonEmpty: Boolean = !isEmpty
  
  /** Map the endpoints of this [[Interval]]. */
  def map [Y: OrderedRing] (f: X => Y): Interval[Y] = this match
    case NonEmpty(start, end) => Interval(f(start), f(start))
    case Empty() => Empty()
  
  /** Whether this interval is of finite length. */
  def isFinite (using X <:< Unbounded[?]): Boolean = this match
    case NonEmpty(start, end) => start.isFinite && end.isFinite
    case Empty() => true
  
  /** Whether this interval is of infinite length. */
  def isInfinite (using X <:< Unbounded[?]): Boolean = !isFinite
    
  override def toString = this match
    case NonEmpty(start, end) => s"($start, $end)"
    case Empty() => "∅"

object Interval:
  
  /** A finite interval over the integers. */ type IntervalI = Interval[Int]
  /** A finite interval over the longs. */ type IntervalL = Interval[Long]
  /** A finite interval over the floats. */ type IntervalF = Interval[Float]
  /** A finite interval over the doubles. */ type IntervalD = Interval[Double]
  type IntervalR = Interval[Rational]
  type IntervalS = Interval[Surd]
  
  /** A possibly-unbounded interval. */ type UInterval[X] = Interval[Unbounded[X]]
  /** A possibly-unbounded interval over the integers. */ type UIntervalI = Interval[UInt]
  /** A possibly-unbounded interval over the longs. */ type UIntervalL = Interval[ULong]
  /** A possibly-unbounded interval over the floats. */ type UIntervalF = Interval[UFloat]
  /** A possibly-unbounded interval over the doubles. */ type UIntervalD = Interval[UDouble]
  type UIntervalR = Interval[URational]
  type UIntervalS = Interval[USurd]
  
  /** Create an interval between the given start and end, which will be empty if start > end. */
  def apply [X: OrderedRing] (start: X, end: X): Interval[X] =
    if start <= end
    then NonEmpty(start, end)
    else Empty()
    
  /** Create the minimal interval to contain all the given points. */
  def bounding [X: OrderedRing] (vs: X*): Interval[X] =
    Interval(vs.min, vs.max)
  
  /** Create an empty interval. */
  def none [X: OrderedRing]: Interval[X] = Interval.Empty()
  
  object UInterval:
    
    /** Create an interval between the given start and end, which will be empty if start > end. */
    def apply [X: OrderedRing] (start: X, end: X): UInterval[X] =
      if start < end
      then NonEmpty(Finite(start), Finite(end))
      else Empty()
  
    /** Create an interval from the given start to positive infinity. */
    def from [X: OrderedRing] (start: X): UInterval[X] =
      Interval(Finite(start), PositiveInfinity)
      
    /** Create an interval from negative infinity to the given end. */
    def to [X: OrderedRing] (end: X): UInterval[X] =
      Interval(NegativeInfinity, Finite(end))
      
    /** Create an infinite interval containing all points. */
    def all [X: OrderedRing]: UInterval[X] = Interval(NegativeInfinity, PositiveInfinity)
  
  given [X: OrderedRing as R]: Group[Interval[X]] with
    
    // Intervals can be added using the Minkowski sum.
    def sum (x: Interval[X], y: Interval[X]): Interval[X] = (x, y) match
      case (NonEmpty(s1, e1), NonEmpty(s2, e2)) => NonEmpty(s1 + s2, e1 + e2)
      case _ => Empty()
    
    // Adding the interval containing only zero leaves the interval unchanged.
    def additiveIdentity: Interval[X] =
      Interval(R.additiveIdentity, R.additiveIdentity)
    
    // To negate an interval is to flip it over the origin.
    def additiveInverse (x: Interval[X]): Interval[X] = x match
      case NonEmpty(start, end) => NonEmpty(-end, -start)
      case Empty() => Empty()