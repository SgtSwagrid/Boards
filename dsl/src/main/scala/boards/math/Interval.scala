package boards.math

import boards.math.algebra.Algebra.{*, given}
import boards.math.Interval.*
import boards.math.ops.AffineOps.*
import boards.math.algebra.Unbounded.{Finite, NegativeInfinity, PositiveInfinity}
import boards.math.algebra.Unbounded
import boards.math.ops.MinkowskiOps.Difference
import boards.math.ops.SetOps.{Intersection, Union}
import boards.math.ops.TransformOps.UScalarFunctor
import io.circe.*
import io.circe.generic.auto.*

import scala.deriving.Mirror

/** Represents a set of scalar values in some contiguous interval.
  * @tparam X The discrete field over which the interval is defined.
  */
enum Interval [@specialized X: Numeric as R] extends
  Scalar[X, Interval[X]],
  Difference[Interval[X]],
  Union[Interval[X]],
  Intersection[Interval[X]],
  UScalarFunctor[X, Interval]:
  
  case All     [Y: Numeric] ()           extends Interval[Y]
  case From    [Y: Numeric] (s: Y)       extends Interval[Y]
  case To      [Y: Numeric] (e: Y)       extends Interval[Y]
  case Between [Y: Numeric] (s: Y, e: Y) extends Interval[Y]
  case Point   [Y: Numeric] (p: Y)       extends Interval[Y]
  case Empty   [Y: Numeric] ()           extends Interval[Y]
  
  def ustart: Unbounded[X] = this match
    case From(start) => Finite(start)
    case Between(start, _) => Finite(start)
    case Point(point) => Finite(point)
    case To(_) | All() => NegativeInfinity
    case Empty() => PositiveInfinity
    
  def start: X = ustart.toFinite
    
  def uend: Unbounded[X] = this match
    case To(end) => Finite(end)
    case Between(_, end) => Finite(end)
    case Point(point) => Finite(point)
    case From(_) | All() => PositiveInfinity
    case Empty() => NegativeInfinity
    
  def end: X = uend.toFinite
    
  /** Determines the length of this interval. */
  def ulength: Unbounded[X] = this match
    case Between(start, end) => Finite(end - start + R.one)
    case Point(_) => Finite(R.one)
    case Empty() => Finite(R.zero)
    case _ => PositiveInfinity
    
  def length: X = ulength.toFinite
  
  def enumerate: LazyList[X] = this match
    case All() => R.zero +: LazyList.iterate(R.one)(_ + R.one).flatMap(x => Seq(x, -x))
    case From(start) => LazyList.iterate(start)(_ + R.one)
    case To(end) => LazyList.iterate(end)(_ - R.one)
    case Between(start, end) => LazyList.iterate(start)(_ + R.one).takeWhile(_ <= end)
    case Point(point) => LazyList(point)
    case Empty() => LazyList.empty
  
  /** Determines whether the given point is contained within this interval. */
  def contains (x: X): Boolean = this match
    case All() => true
    case From(start) => x >= start
    case To(end) => x <= end
    case Between(start, end) => start <= x && x <= end
    case Point(point) => x == point
    case Empty() => false
  
  def contains (that: Interval[X]): Boolean = (this, that) match
    case (All(), _) => true
    case (_, All()) => false
    case (_, Empty()) => true
    case (Empty(), _) => false
    case (From(s1), From(s2)) => s1 <= s2
    case (To(e1), To(e2)) => e1 >= e2
    case (Between(s1, e1), Between(s2, e2)) => s1 <= s2 && e1 >= e2
    case (From(s1), Between(s2, _)) => s1 <= s2
    case (To(e1), Between(_, e2)) => e1 >= e2
    case (_, Point(p)) => contains(p)
    case _ => false
  
  def map [Y: Numeric] (f: Unbounded[X] => Unbounded[Y]): Interval[Y] = this match
    case All() => Interval.ubounding(f(NegativeInfinity), f(PositiveInfinity))
    case From(start) => Interval.ubounding(f(Finite(start)), f(PositiveInfinity))
    case To(end) => Interval.ubounding(f(Finite(end)), f(NegativeInfinity))
    case Between(start, end) => Interval.ubounding(f(Finite(start)), f(Finite(end)))
    case Point(point) => Interval.ubounding(f(Finite(point)))
    case Empty() => Interval.empty
  
  /** Produces the minimal interval to contain both operands. */
  def union (that: Interval[X]): Interval[X] = (this, that) match
    case (All(), _) => Interval.all
    case (From(s1), From(s2)) => Interval.from(s1 min s2)
    case (To(e1), To(e2)) => Interval.to(e1 max e2)
    case (From(_), To(_)) => Interval.all
    case (Between(s1, e1), Between(s2, e2)) => Interval.between(s1 min s2, e1 max e2)
    case (From(s1), Between(s2, _)) => Interval.from(s1 min s2)
    case (To(e1), Between(_, e2)) => Interval.to(e1 max e2)
    case (Point(p1), Point(p2)) if p1 == p2 => Interval.point(p1)
    case (Point(p1), Point(p2)) if p1 != p2 => Interval.between(p1 min p2, p1 max p2)
    case (Point(p), From(s)) => Interval.from(p min s)
    case (Point(p), To(e)) => Interval.to(p max e)
    case (Point(p), Between(s, e)) => Interval.between(p min s, p max e)
    case (Empty(), i) => i
    case _ => that | this
  
  /** Produces the maximal interval contained in both operands. */
  def intersect (that: Interval[X]): Interval[X] = (this, that) match
    case (All(), i) => i
    case (From(s1), From(s2)) => Interval.from(s1 max s2)
    case (To(e1), To(e2)) => Interval.to(e1 min e2)
    case (From(s), To(e)) => Interval.between(s, e)
    case (Between(s1, e1), Between(s2, e2)) => Interval.between(s1 max s2, e1 min e2)
    case (From(s1), Between(s2, e2)) => Interval.between(s1 max s2, e2)
    case (To(e1), Between(s2, e2)) => Interval.between(s2, e1 min e2)
    case (Point(p), i) if i.contains(p) => this
    case (Point(p), i) if !i.contains(p) => Interval.empty
    case (Empty(), _) => Interval.empty
    case _ => that & this
  
  def minkowskiSum (that: Interval[X]): Interval[X] = (this, that) match
    case (All(), _) => Interval.all
    case (From(s1), From(s2)) => Interval.from(s1 + s2)
    case (To(e1), To(e2)) => Interval.to(e1 + e2)
    case (From(_), To(_)) => Interval.all
    case (Between(s1, e1), Between(s2, e2)) => Interval.between(s1 + s2, e2 + e2)
    case (Between(s1, _), From(s2)) => Interval.from(s1 + s2)
    case (Between(_, e1), To(e2)) => Interval.to(e1 + e2)
    case (Point(p), i) => i + p
    case (Empty(), _) => Interval.empty
    case _ => that + this
  
  def negate: Interval[X] = this match
    case All() => Interval.all
    case From(start) => Interval.to(-start)
    case To(end) => Interval.from(-end)
    case Between(start, end) => Interval.between(-end, -start)
    case Point(point) => Interval.point(-point)
    case Empty() => Interval.empty
    
  /** Shift the entire interval by the given offset. */
  def shift (x: X): Interval[X] = this match
    case All() => Interval.all
    case From(start) => Interval.from(start + x)
    case To(end) => Interval.to(end + x)
    case Between(start, end) => Interval.between(start + x, end + x)
    case Point(point) => Interval.point(point + x)
    case Empty() => Interval.empty
    
  def scale (factor: X): Interval[X] = this match
    case All() => Interval.all
    case From(start) => Interval.from(start * factor)
    case To(end) => Interval.to(end * factor)
    case Between(start, end) => Interval.between(start * factor, end * factor)
    case Point(point) => Interval.point(point * factor)
    case Empty() => Interval.empty
  
  /** Shift the start of the interval by the given offset. */
  def shiftStart (x: X): Interval[X] = this match
    case From(start) => From(start + x)
    case Between(start, end) => Interval.between(start + x, end)
    case Point(point) => Interval.between(point + x, point)
    case _ => this
  
  /** Shift the end of the interval by the given offset. */
  def shiftEnd (x: X): Interval[X] = this match
    case To(end) => To(end + x)
    case Between(start, end) => Interval.between(start, end + x)
    case Point(point) => Interval.between(point, point + x)
    case _ => this
  
  /** Take only the portion of the interval which lies before the given value. */
  def limitBelow (x: X): Interval[X] =
    this & Interval.to(x)
    
  def limitBelow (x: Unbounded[X]): Interval[X] = x match
    case Finite(x) => limitBelow(x)
    case PositiveInfinity => this
    case NegativeInfinity => Interval.empty
  
  /** Take only the portion of the interval which lies after the given value. */
  def limitAbove (x: X): Interval[X] =
    this & Interval.from(x)
  
  def limitAbove (x: Unbounded[X]): Interval[X] = x match
    case Finite(x) => limitAbove(x)
    case PositiveInfinity => Interval.empty
    case NegativeInfinity => this
    
  def extendStart (x: X): Interval[X] = this match
    case From(start) => Interval.from(start - x)
    case Between(start, end) => Interval.between(start - x, end)
    case Point(point) => Interval.between(point - x, point)
    case _ => this
    
  def extendEnd (x: X): Interval[X] = this match
    case To(end) => Interval.to(end + x)
    case Between(start, end) => Interval.between(start, end + x)
    case Point(point) => Interval.between(point, point + x)
    case _ => this
    
  def extend (x: X): Interval[X] = extendStart(x).extendEnd(x)
  
  /** Provide an alternative value in case this interval is empty. */
  def orElse (x: X): Interval[X] = this match
    case Empty() => Point(x)
    case _ => this
  
  /** Whether this interval contains no values. */
  def isEmpty: Boolean = this match
    case Empty() => true
    case _ => false
  
  /** Whether this interval contains at least one point. */
  def nonEmpty: Boolean = !isEmpty
  
  def isFinite: Boolean = this match
    case All() | From(_) | To(_) => false
    case Between(_, _) | Point(_) | Empty() => true
    
  def isInfinite: Boolean = !isFinite
  
  override def toString = this match
    case All() => "U"
    case From(start) => s"[$start, ∞)"
    case To(end) => s"(-∞, $end]"
    case Between(start, end) => s"[$start, $end]"
    case Point(point) => point.toString
    case Empty() => "∅"

object Interval:
  
  /** A finite interval over the integers. */
  type IntervalI = Interval[Int]
  /** A finite interval over the longs. */
  type IntervalL = Interval[Long]
  /** A finite interval over the floats. */
  type IntervalF = Interval[Float]
  /** A finite interval over the doubles. */
  type IntervalD = Interval[Double]
  type IntervalR = Interval[Rational]
  
  type NonEmpty [X] = All[X] | From[X] | To[X] | Between[X] | Point[X]
  type Finite [X] = Between[X] | Point[X] | Empty[X]
  type Infinite [X] = All[X] | From[X] | To[X]
  type NonEmptyFinite [X] = Between[X] | Point[X]
  
  /** Create an interval between the given start and end, which will be empty if start > end. */
  def between [X: Numeric] (start: X, end: X): Interval[X] =
    if start < end then Between(start, end)
    else if start == end then Point(start)
    else Empty()
    
  def ubetween [X: Numeric] (start: Unbounded[X], end: Unbounded[X]): Interval[X] =
    (start, end) match
      case (Finite(x1), Finite(x2)) => Interval.between(x1, x2)
      case (NegativeInfinity, Finite(x)) => Interval.to(x)
      case (Finite(x), PositiveInfinity) => Interval.from(x)
      case (NegativeInfinity, PositiveInfinity) => Interval.all
      case _ => Interval.empty
  
  /** Create an empty interval. */
  def empty [X: Numeric]: Interval[X] = Empty()
  
  def all [X: Numeric]: Interval[X] = All()
  
  def from [X: Numeric] (start: X): Interval[X] = From(start)
  def positive [X: Numeric as N]: Interval[X] = from(N.zero)
  def negative [X: Numeric as N]: Interval[X] = to(N.zero)
  
  def to [X: Numeric] (end: X): Interval[X] = To(end)
  
  def point [X: Numeric] (point: X): Interval[X] = Point(point)
    
  /** Create the minimal interval to contain all the given points. */
  def bounding [X: Numeric] (xs: X*): Interval[X] =
    if xs.nonEmpty then Interval.between(xs.min, xs.max) else Interval.empty
    
  def ubounding [X: Numeric] (xs: Unbounded[X]*): Interval[X] =
    if xs.nonEmpty then Interval.ubetween(xs.min, xs.max) else Interval.empty
    
  given [X: {Numeric, Encoder}]: Encoder[Interval[X]] =
    summon[Encoder[(Unbounded[X], Unbounded[X])]]
      .contramap(i => (i.ustart, i.uend))
    
  given [X: {Numeric, Decoder}]: Decoder[Interval[X]] =
    summon[Decoder[(Unbounded[X], Unbounded[X])]]
      .map(Interval.ubetween)