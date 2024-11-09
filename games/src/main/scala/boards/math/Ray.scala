package boards.math

import boards.imports.math.{*, given}
import boards.math.Region.Shape
import boards.util.extensions.FunctionOps.*

import java.lang.Math.{max, min}

/** A `Region` consisting of those points which lie in one of several directions from some source point(s). */
sealed trait Ray extends Shape[Int]:
  
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
  def takeWhile(f: VecI => Boolean): Ray = takeUntil(!f)
  /** In each direction, only include positions before `f` is satisfied. */
  def takeUntil(f: VecI => Boolean): Ray = takeTo(f).retractWhile(f)
  /** In each direction, only include positions up to and including the point where `f` is first satisfied. */
  def takeTo(f: VecI => Boolean): Ray
  
  /** In each direction, remove the furthest `n` positions. */
  def retract(n: Int): Ray
  /** In each direction, remove the furthest positions as long as `f` is satisfied. */
  def retractWhile(f: VecI => Boolean): Ray
  /** In each direction, remove the furthest positions until `f` is satisfied. */
  def retractUntil(f: VecI => Boolean): Ray = retractWhile(!f)
  /** In each direction, remove the furthest positions up to and including the point where `f` is first satisfied. */
  def retractTo(f: VecI => Boolean): Ray = retractUntil(f).retract(1)
  
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
      yield SegmentRay(source + (if inclusive then VecI.empty else direction), direction)
  
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
      yield SegmentRay(from, from.directionTo(to), Some(from.stepsTo(to) + 1))
  
  /**
   * A single-source, single-direction, possibly infinite ray.
   * @param source The starting point of the ray.
   * @param direction The direction in which the ray extends.
   * @param limit The maximum number of positions in the ray.
   */
  private class SegmentRay (
    source: VecI,
    direction: VecI,
    limit: Option[Int] = None,
  ) extends Ray:
    
    def positions: Iterator[VecI] =
      if direction.isZero then Iterator.single(source) else
        limit match
          case Some(limit) => Iterator.iterate(source)(_ + direction).take(limit)
          case None => Iterator.iterate(source)(_ + direction)
    
    def contains(v: VecI): Boolean =
      (v - source).isMultipleOf(direction).exists: factor =>
        factor >= 0 && limit.forall(factor < _) &&
          positions.nextOption.exists: next =>
            (next - source).isMultipleOf(direction).exists: steps =>
              steps <= factor && positions.drop(factor - steps).nonEmpty
            
    override def start: VecI = source
    override def end: VecI = source
    
    def drop(n: Int) =
      val me = this
      new SegmentRay(source + (direction * n), direction, limit.map(limit => max(0, limit - n)))
        { override def positions = me.positions.drop(n) }
      
    def dropWhile(f: VecI => Boolean) =
      val me = this
      new SegmentRay(source, direction, limit)
        { override def positions = me.positions.dropWhile(f) }
      
    def take(n: Int) =
      val me = this
      new SegmentRay(source, direction, Some(min(limit.getOrElse(n), n)))
        { override def positions = me.positions.take(n) }
      
    def takeTo(f: VecI => Boolean) =
      val me = this
      new SegmentRay(source, direction, limit)
        { override def positions = me.positions.takeWhile(!f) ++ me.positions.dropWhile(!f).take(1) }
    
    def retract(n: Int) =
      val me = this
      positions.knownSize match
        case -1 =>
          new SegmentRay(source, direction, limit.map(limit => max(0, limit - n)))
            { override def positions = me.positions.sliding(n + 1).withPartial(false).map(_.head) }
        case size => take(size - n)
      
    def retractWhile(f: VecI => Boolean) =
      val me = this
      def find(it: Iterator[VecI], f: VecI => Boolean): Iterator[VecI] =
        val a = it.takeWhile(!f)
        val bc = it.dropWhile(!f)
        val b = bc.takeWhile(f)
        val c = bc.dropWhile(f)
        a ++ (if c.nonEmpty then b ++ find(c, f) else Iterator.empty)
      new SegmentRay(source, direction, limit) {
        override def positions = me.positions.knownSize match
          case -1 => find(me.positions, f)
          case _ => me.positions.toSeq.reverse.dropWhile(f).reverseIterator
      }
  
  /**
   * A multiple-source, multiple-direction collection of rays.
   * @param parts The sub-rays which make up this composite.
   */
  private class CompositeRay (
    parts: Seq[Ray],
  ) extends Ray:
    
    override def positions: Iterator[VecI] = parts.iterator.flatMap(_.positions).distinct
    override def contains(v: VecI): Boolean = parts.exists(_.contains(v))
    
    override lazy val start: VecI = parts.map(_.start).reduce(_.zip(_)(Math.min))
    override lazy val end: VecI = parts.map(_.start).reduce(_.zip(_)(Math.max))
    
    def drop(n: Int) = CompositeRay(parts.map(_.drop(n)))
    def dropWhile(f: VecI => Boolean) = CompositeRay(parts.map(_.dropWhile(f)))
  
    def take(n: Int) = CompositeRay(parts.map(_.take(n)))
    def takeTo(f: VecI => Boolean) = CompositeRay(parts.map(_.takeTo(f)))
    
    def retract(n: Int) = CompositeRay(parts.map(_.retract(n)))
    def retractWhile(f: VecI => Boolean) = CompositeRay(parts.map(_.retractWhile(f)))