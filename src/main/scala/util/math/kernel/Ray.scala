package util.math.kernel

import util.math.Pos
import util.math.Pos.{*, given}
import util.math.kernel.Kernel.{*, given}
import util.extensions.FunctionOps.*
import util.math.Number.*

sealed trait Ray extends Shape:
  
  def drop(n: Int): Ray
  def dropWhile(f: Pos => Boolean): Ray
  def dropUntil(f: Pos => Boolean): Ray = dropWhile(!f)
  def dropTo(f: Pos => Boolean): Ray = dropUntil(f).drop(1)
  
  def take(n: Int): Ray
  def takeWhile(f: Pos => Boolean): Ray
  def takeUntil(f: Pos => Boolean): Ray = takeWhile(!f)
  def takeTo(f: Pos => Boolean): Ray
  
  def extend(n: Int): Ray
  def extendWhile(f: Pos => Boolean): Ray
  def extendUntil(f: Pos => Boolean): Ray = extendWhile(!f)
  def extendTo(f: Pos => Boolean): Ray = extendUntil(f).extend(1)
  
  def retract(n: Int): Ray
  //def retractWhile(f: Pos => Boolean): Ray
  //def retractUntil(f: Pos => Boolean): Ray = limitWhile(!f)
  //def retractTo(f: Pos => Boolean): Ray = dropUntil(f).drop(1)
  
  def first: Ray = take(1)
  def last: Ray
  def interior: Ray = drop(1).retract(1)
  
object Ray:
  
  def apply (
    directions: Dir,
    inclusive: Boolean = false
  ) (using
    start: Pos = Pos.zero,
    domain: Kernel[?] = Kernel.infinite
  ): Ray =
    Ray.from(start, directions, inclusive)
  
  def from (
    start: Pos = Pos.zero,
    directions: Dir,
    inclusive: Boolean = false
  ) (
    using domain: Kernel[?] = Kernel.infinite
  ): Ray =
    
    val ray = MultiRay(directions.positions(start.dim).positions.toSeq.map: d =>
      SegmentRay(if inclusive then start else start + d, d))
    ray.takeWhile(domain.contains)
    
  def between(from: Pos, to: Pos): SegmentRay =
    val dir = from.directionTo(to)
    val count = gcd((to - from).toSeq.map(_.abs)*) + 1
    SegmentRay(from, dir, count)
  
  case class SegmentRay (
    start: Pos,
    direction: Pos,
    count: Int = 1000,
  ) extends Ray:
    
    private def find(f: Pos => Boolean, from: Int = 0, until: Int = count): Int =
      val itr = Iterator.iterate(start)(_ + direction).drop(from)
      val itr_ltd = if until == -1 then itr else itr.take(until - from)
      val i = itr_ltd.indexWhere(f)
      if i == -1 then until else from + i
    
    def positions: Iterator[Pos] =
      Iterator.iterate(start)(_ + direction).take(count)
    
    def contains(v: Pos): Boolean =
      val z = (v - start).toSeq.zip(direction.toSeq)
      val steps = z.filter((_, d) => d != 0).map(_ / _)
      steps.forall(_ == steps.head) &&
        z.forall((v, d) => (d == 0 && v == 0) || (d != 0 && v % d == 0))
    
    def drop(n: Int): SegmentRay = SegmentRay(start + (direction * n), direction, Math.max(count - n, 0))
    def dropWhile(f: Pos => Boolean): SegmentRay = drop(find(f))
    def take(n: Int): SegmentRay = SegmentRay(start, direction, Math.min(n, count))
    def takeWhile(f: Pos => Boolean): SegmentRay = take(find(!f))
    def takeTo(f: Pos => Boolean): SegmentRay = take(find(f) + 1)
    def extend(n: Int): SegmentRay = SegmentRay(start, direction, count + n)
    def extendWhile(f: Pos => Boolean): SegmentRay = extend(find(!f, count, -1) - count)
    def retract(n: Int): SegmentRay = SegmentRay(start, direction, Math.max(count - n, 0))
    
    def last: SegmentRay = drop(count - 1)
    
    val offset: Pos = start.zip(start + (direction * (count - 1)))(Math.min)
    val size: Pos = direction.map(_.abs) * (count - 1)
    
    override def translate(v: Pos): SegmentRay = copy(start = start + v)
    override def rotate(from: Int, to: Int): SegmentRay = copy(direction = direction.rotate(from, to))
    
  case class MultiRay (
    rays: Seq[SegmentRay]
  ) extends Ray:
    
    def positions: Iterator[Pos] = rays.iterator.flatMap(_.positions).distinct
    def contains(v: Pos): Boolean = rays.exists(_.contains(v))
    
    def drop(n: Int): Ray = MultiRay(rays.map(_.drop(n)))
    def dropWhile(f: Pos => Boolean): Ray = MultiRay(rays.map(_.dropWhile(f)))
    def take(n: Int): Ray = MultiRay(rays.map(_.take(n)))
    def takeWhile(f: Pos => Boolean): Ray = MultiRay(rays.map(_.takeWhile(f)))
    def takeTo(f: Pos => Boolean): Ray = MultiRay(rays.map(_.takeTo(f)))
    def extend(n: Int): Ray = MultiRay(rays.map(_.extend(n)))
    def extendWhile(f: Pos => Boolean): Ray = MultiRay(rays.map(_.extendWhile(f)))
    def retract(n: Int): Ray = MultiRay(rays.map(_.retract(n)))
    
    def last: Ray = MultiRay(rays.map(_.last))
    
    val offset: Pos = rays.map(_.offset).reduce(_.zip(_)(Math.min))
    val size: Pos = rays.map(_.offset).reduce(_.zip(_)(Math.max)) - offset
    
    override def translate(v: Pos): MultiRay = copy(rays = rays.map(_.translate(v)))
    override def rotate(from: Int, to: Int): MultiRay = copy(rays = rays.map(_.rotate(from, to)))