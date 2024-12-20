package boards.math.region

import boards.imports.math.{*, given}
import Region.HasRegionI
import Vec.HasVecI
import boards.math.Bijection.Translate
import boards.math.Interval
import boards.math.Interval.{UInterval, UIntervalI}
import boards.math.WithInfinity.{ExtendedInt, Finite, NegativeInfinity, PositiveInfinity}
import boards.math.region.BoundingBox.{BoundingBoxI, UBoundingBox, UBoundingBoxI}
import boards.math.region.RegionOps.WindowOps
import boards.util.extensions.FunctionOps.*

import java.lang.Math.{max, min}
import scala.annotation.tailrec

/** A `Region` consisting of those points which lie in one of several directions from some source point(s). */
sealed trait Ray extends Region[Int]:
  
  /** In each direction, remove the nearest `n` positions. */
  def drop(n: Int): Ray
  /** In each direction, remove the nearest positions as long as `f` is satisfied. */
  def dropWhile(f: VecI => Boolean): Ray
  /** In each direction, remove the nearest positions until `f` is satisfied. */
  def dropUntil(f: VecI => Boolean): Ray = dropWhile(!f)
  /** In each direction, remove the nearest positions up to and including the point where `f` is first satisfied. */
  def dropTo(f: VecI => Boolean): Ray = dropUntil(f).drop(1)
  
  /** In each direction, only include the nearest `n` positions. */
  def take(n: Int): Ray
  /** In each direction, only include positions as long as `f` is satisfied. */
  def takeWhile(f: VecI => Boolean): Ray
  /** In each direction, only include positions before `f` is satisfied. */
  def takeUntil(f: VecI => Boolean): Ray = takeWhile(!f)
  /** In each direction, only include positions up to and including the point where `f` is first satisfied. */
  def takeTo(f: VecI => Boolean): Ray
  
  /** In each direction, remove the furthest `n` positions. */
  def retract(n: Int): Ray
  
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
    source: HasRegionI,
    direction: HasRegionI,
    inclusive: Boolean = false,
  ): Ray =
    CompositeRay:
      for
        source <- source.region.positions.toSeq
        direction <- direction.region.positions.toSeq
        ray = SegmentRay(source, direction)()
      yield if inclusive then ray else ray.drop(1)
  
  /**
   * Create a new ray consisting of all points on the line between `from` and `to`, inclusive of both ends.
   * @param from The starting point(s) of the ray.
   * @param to The ending point(s) of the ray.
   */
  def between (
    from: HasRegionI,
    to: HasRegionI,
  ): Ray =
    CompositeRay:
      for
        from <- from.region.positions.toSeq
        to <- to.region.positions.toSeq
        ray = SegmentRay(from, from.directionTo(to))()
      yield ray.take(from.stepsTo(to) + 1)
      
  private class SegmentRay (
    val source: VecI,
    val direction: VecI,
    val bounds: UIntervalI = UInterval.from(0),
    val length: UIntervalI = Interval(PositiveInfinity, PositiveInfinity),
  ) (
    pos: => LazyList[VecI] =
      if direction.isZero
      then LazyList(source)
      else LazyList.iterate(source)(_ + direction)
  ) extends Ray:
    
    lazy val positions = pos
    
    def contains(v: HasVecI) =
      (v.position - source).asMultipleOf(direction).exists: a =>
        bounds.contains(a) && positions.headOption.exists: next =>
          (next - source).asMultipleOf(direction).exists: b =>
            b <= a && positions.lengthIs > (a - b)
      
    val boundingBox = bounds match
      case Interval.NonEmpty(start, end) => BoundingBox.of (
        source.asUnbounded + (direction.asUnbounded * start),
        source.asUnbounded + (direction.asUnbounded * end),
      )
      case Interval.Empty() => UBoundingBoxI.empty
      
    private def refine(bounds: UIntervalI, length: UIntervalI, positions: => LazyList[VecI]): SegmentRay =
      SegmentRay(source, direction, bounds, length)(positions)
    
    def drop(n: Int) =
      refine(bounds.shiftStart(n), length.shift(-n).after(0).orElse(0), positions.drop(n))
      
    def dropWhile(f: VecI => Boolean) =
      refine(bounds, length, positions.dropWhile(f))
      
    def take(n: Int) =
      refine(bounds, length.before(n).orElse(n), positions.take(n))
    
    def takeWhile(f: VecI => Boolean) =
      refine(bounds, length, positions.takeWhile(f))
      
    def takeTo(f: VecI => Boolean) =
      val (prefix, suffix) = positions.span(!f)
      refine(bounds, length, prefix.lazyAppendedAll(suffix.headOption))
      
    def retract(n: Int) =
      refine(bounds.shiftEnd(-n), length.shift(-n).after(0).orElse(0), positions.dropRight(n))
    
    @tailrec
    private def steps(step: Int, interval: UIntervalI): UIntervalI =
      if step < 0 then steps(-step, -interval)
      else if step == 0 then (if interval.contains(0) then UInterval.from(0) else Interval.none)
      else interval.after(0) match
        case Interval.NonEmpty(Finite(start), Finite(end)) =>
          UInterval((start + (step - 1)) / step, end / step)
        case Interval.NonEmpty(Finite(start), PositiveInfinity) =>
          Interval(Finite((start + (step - 1)) / step), PositiveInfinity)
        case _ => Interval.none
    
    private def multiples(target: UBoundingBoxI): UIntervalI =
      target.map(Translate(-source).infinite) match
        case boundingBox @ BoundingBox.NonEmpty(_, _) =>
          direction.components.zip(boundingBox.intervals)
            .map(steps)
            .foldLeft(UInterval.from(0))(_ & _)
        case BoundingBox.Empty() => Interval.none
      
    override def window(window: UBoundingBoxI): Ray =
      val bounds = this.bounds & multiples(window)
      lazy val positions = bounds match
        case Interval.NonEmpty(_, _) =>
          this.positions.dropWhile(v => !window.inBounds(v)).takeWhile(window.inBounds)
        case Interval.Empty() => LazyList.empty
      refine(bounds, length.before(bounds.length).orElse(bounds.length), positions)
  
  /**
   * A multiple-source, multiple-direction collection of rays.
   * @param parts The sub-rays which make up this composite.
   */
  private class CompositeRay (
    parts: Seq[Ray],
  ) extends Ray:
    
    override lazy val positions: LazyList[VecI] = LazyList.from(parts).flatMap(_.positions).distinct
    override def contains(v: HasVecI): Boolean = parts.exists(_.contains(v))
    
    lazy val boundingBox =
      parts.map(_.boundingBox).reduceOption(_ | _).getOrElse(UBoundingBoxI.empty)
    
    def drop(n: Int) = CompositeRay(parts.map(_.drop(n)))
    def dropWhile(f: VecI => Boolean) = CompositeRay(parts.map(_.dropWhile(f)))
  
    def take(n: Int) = CompositeRay(parts.map(_.take(n)))
    def takeWhile(f: VecI => Boolean) = CompositeRay(parts.map(_.takeWhile(f)))
    def takeTo(f: VecI => Boolean) = CompositeRay(parts.map(_.takeTo(f)))
    
    def retract(n: Int) = CompositeRay(parts.map(_.retract(n)))
    
    override def window(window: UBoundingBoxI): RegionI =
      parts.map(_.window(window)).reduceOption(_ | _).getOrElse(Region.empty)