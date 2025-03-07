package boards.math.vector

import boards.math.vector.Region.RegionI
import boards.math.vector.Vec.VecI
import boards.math.Interval
import boards.math.algebra.Bijection
import boards.math.algebra.Algebra.{*, given}
import boards.math.Conversions.{*, given}
import boards.math.Interval.IntervalI
import boards.math.algebra.Unbounded.{Finite, PositiveInfinity}
import boards.math.vector.Bounds.BoundsI
import boards.util.extensions.FunctionOps.unary_!

import scala.annotation.tailrec

/** A `Region` consisting of those points which lie in one of several directions from some source point(s). */
sealed trait Ray extends Region[Int]:
  
  /** In each direction, remove the nearest `n` positions. */
  def drop (n: Int): Ray
  /** In each direction, remove the nearest positions as long as `f` is satisfied. */
  def dropWhile (f: VecI => Boolean): Ray
  /** In each direction, remove the nearest positions until `f` is satisfied. */
  def dropUntil (f: VecI => Boolean): Ray = dropWhile(!f)
  /** In each direction, remove the nearest positions up to and including the point where `f` is first satisfied. */
  def dropTo (f: VecI => Boolean): Ray = dropUntil(f).drop(1)
  
  /** In each direction, only include the nearest `n` positions. */
  def take (n: Int): Ray
  /** In each direction, only include positions as long as `f` is satisfied. */
  def takeWhile (f: VecI => Boolean): Ray
  /** In each direction, only include positions before `f` is satisfied. */
  def takeUntil (f: VecI => Boolean): Ray = takeWhile(!f)
  /** In each direction, only include positions up to and including the point where `f` is first satisfied. */
  def takeTo (f: VecI => Boolean): Ray
  
  /** In each direction, remove the furthest `n` positions. */
  def retract (n: Int): Ray
  
  /** Remove the first and last position along this ray. */
  def interior: Ray = drop(1).retract(1)
  
object Ray:
  
  /**
   * Create a ray consisting of all points along some direction(s) from a source.
   * @param source The starting point(s) of the ray.
   * @param direction The direction(s) in which the ray extends.
   * @param inclusive Whether `source` itself is included (default=false).
   */
  def from (
    source: RegionI,
    direction: RegionI,
    inclusive: Boolean = false,
  ): Ray =
    CompositeRay:
      for
        source <- source.positions.toSeq
        direction <- direction.positions.toSeq
        ray = SegmentRay(source, direction)()
      yield if inclusive then ray else ray.drop(1)
  
  /**
   * Create a new ray consisting of all points on the line between `from` and `to`, inclusive of both ends.
   * @param from The starting point(s) of the ray.
   * @param to The ending point(s) of the ray.
   */
  def between (
    from: RegionI,
    to: RegionI,
  ): Ray =
    CompositeRay:
      for
        from <- from.positions.toSeq
        to <- to.positions.toSeq
        ray = SegmentRay(from, from.directionTo(to))()
      yield ray.take(from.stepsTo(to) + 1)
      
  private class SegmentRay (
    val source: VecI,
    val direction: VecI,
    val lengthBounds: IntervalI = Interval.positive,
    val length: IntervalI = Interval.all,
  ) (
    pos: => LazyList[VecI] =
      if direction.isZero
      then LazyList(source)
      else LazyList.iterate(source)(_ + direction)
  ) extends Ray:
    
    lazy val positions = pos
    
    def contains (v: VecI) =
      (v.position - source).asMultipleOf(direction).exists: a =>
        lengthBounds.contains(a) && positions.headOption.exists: next =>
          (next - source).asMultipleOf(direction).exists: b =>
            b <= a && positions.lengthIs > (a - b)
      
    val bounds =
      if lengthBounds.isEmpty then Bounds.empty else
        Bounds.ubounding (
          source.toUnbounded + (direction.toUnbounded * lengthBounds.ustart),
          source.toUnbounded + (direction.toUnbounded * lengthBounds.uend),
        )
      
    private def refine (bounds: IntervalI, length: IntervalI, positions: => LazyList[VecI]): SegmentRay =
      SegmentRay(source, direction, bounds, length)(positions)
    
    def drop (n: Int) =
      refine(lengthBounds.shiftStart(n), length.shift(-n).limitAbove(0).orElse(0), positions.drop(n))
      
    def dropWhile (f: VecI => Boolean) =
      refine(lengthBounds, length, positions.dropWhile(f))
      
    def take (n: Int) =
      refine(lengthBounds, length.limitBelow(n).orElse(n), positions.take(n))
    
    def takeWhile (f: VecI => Boolean) =
      refine(lengthBounds, length, positions.takeWhile(f))
      
    def takeTo (f: VecI => Boolean) =
      val (prefix, suffix) = positions.span(!f)
      refine(lengthBounds, length, prefix.lazyAppendedAll(suffix.headOption))
      
    def retract (n: Int) =
      refine(lengthBounds.shiftEnd(-n), length.shift(-n).limitAbove(0).orElse(0), positions.dropRight(n))
    
    /** Determine the number of multiples of step that are required to fall inside the given interval. */
    @tailrec
    private def multiples (step: Int, interval: IntervalI): IntervalI =
      if step < 0 then multiples(-step, -interval)
      else if step == 0 then (if interval.contains(0) then Interval.from(0) else Interval.empty)
      else interval.limitAbove(0) match
        case Interval.Between(start, end) =>
          Interval.between((start + (step - 1)) / step, end / step)
        case Interval.Point(point) =>
          Interval.between((point + (step - 1)) / step, point / step)
        case Interval.From(start) =>
          Interval.from((start + (step - 1)) / step)
        case _ => Interval.empty
    
    /** Determine the number of steps in the current direction one would have to take to be inside the given bounding box. */
    private def steps (target: BoundsI): IntervalI =
      val bounds = target - source
      if bounds.isEmpty then Interval.empty else
        direction.components.zip(bounds.intervals)
          .map(multiples)
          .foldLeft(Interval.from(0))(_ & _)
       
    override def window (window: BoundsI): Ray =
      val bounds = lengthBounds & steps(window)
      lazy val positions = if bounds.isEmpty then LazyList.empty else
        this.positions.dropWhile(v => !window.contains(v)).takeWhile(window.contains)
      refine(bounds, length.limitBelow(bounds.usize).orElse(0), positions)
  
  /**
   * A multiple-source, multiple-direction collection of rays.
   * @param parts The sub-rays which make up this composite.
   */
  private class CompositeRay (
    parts: Seq[Ray],
  ) extends Ray:
    
    override lazy val positions: LazyList[VecI] = LazyList.from(parts).flatMap(_.positions).distinct
    override def contains (v: VecI): Boolean = parts.exists(_.contains(v))
    
    lazy val bounds =
      parts.map(_.bounds).reduceOption(_ | _).getOrElse(Bounds.empty)
    
    def drop (n: Int): Ray = CompositeRay(parts.map(_.drop(n)))
    def dropWhile (f: VecI => Boolean): Ray = CompositeRay(parts.map(_.dropWhile(f)))
  
    def take (n: Int): Ray = CompositeRay(parts.map(_.take(n)))
    def takeWhile (f: VecI => Boolean): Ray = CompositeRay(parts.map(_.takeWhile(f)))
    def takeTo (f: VecI => Boolean): Ray = CompositeRay(parts.map(_.takeTo(f)))
    
    def retract (n: Int): Ray = CompositeRay(parts.map(_.retract(n)))
    
    override def window (window: BoundsI): RegionI =
      parts.map(_.window(window)).reduceOption(_ | _).getOrElse(Region.empty)