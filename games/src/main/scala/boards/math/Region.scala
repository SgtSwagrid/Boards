package boards.math

import boards.imports.math.{*, given}
import boards.math.Algebra.Ring
import boards.math.Align.*

import scala.annotation.{tailrec, targetName}

/**
 * A collection of vectors combined to describe a region in space.
 * @tparam X The discrete field over which the vectors contained therein are defined (usually Int).
 * @tparam T The type of the label assigned to each position, if there is one.
 */
trait Region[@specialized X : Ring : Ordering, +T]:
  import Region.*
  
  /**
   * An iterator over all positions in this region.
   *
   * Note: If the region is infinite, this will not terminate.
   */
  def positions: Iterator[Vec[X]]
  
  /**
   * Whether the given position is contained in this region.
   *
   * See also: 'Region.inBounds'.
   */
  def contains(v: Vec[X]): Boolean
  
  /**
   * Whether the given position is contained in this region.
   *
   * See also: 'Region.inBounds'.
   */
  def contains(v: X*): Boolean = contains(Vec(v*))
  
  def label(v: Vec[X]): Option[T]
  def label(v: X*): Option[T] = label(Vec(v))
  def labels: Iterator[T] = positions.map(label(_).get)
  
  // WARNING: Any implementation of `Region` should override at least 2 of: start, end, or size.
  
  /** The smallest corner of the bounding box containing the entire region. */
  def start: Vec[X] = end - size + Vec.one(Math.max(end.dim, size.dim))
  /** The largest corner of the bounding box containing the entire region. */
  def end: Vec[X] = start + size - Vec.one(Math.max(start.dim, size.dim))
  /** The dimensions of the bounding box containing the entire region. */
  def size: Vec[X] = end - start + Vec.one(Math.max(start.dim, end.dim))
  
  def left: X = start.x; def bottom: X = start.y; def front: X = start.z;
  def right: X = end.x; def top: X = end.y; def back: X = end.z;
  def width: X = size.width; def height: X = size.height; def depth: X = size.depth;
  /** The dimensionality of this region. */
  def dim: Int = size.dim
  
  /** A vector containing only the multiplicative identity. */
  protected final def zeroVec: Vec[X] = Vec.zero(dim)
  /** A vector containing only the additive identity. */
  protected final def oneVec: Vec[X] = Vec.one(dim)
  
  /**
   * Whether the given position is contained in the bounding box that contains this region.
   *
   * See also: 'Region.contains'.
   */
  final def inBounds(v: Vec[X]): Boolean = start <= v && v <= end
  
  /**
   * Whether the given position is contained in the bounding box that contains this region.
   *
   * See also: 'Region.contains'.
   */
  final def inBounds(v: X*): Boolean = inBounds(Vec(v))
  
  /**
   * A view of this region which has been rotated by a quarter turn.
   * @param from The axis to rotate from.
   * @param to The axis to rotate to.
   */
  def rotate(from: Int, to: Int): Region[X, T] = RotatedRegion(this, from, to)
  
  /**
   * A view of this region which has been flipped.
   * @param axis The axis to flip over.
   */
  def flip(axis: Int): Region[X, T] = FlippedRegion(this, axis)
  
  /**
   * Take the cartesian product of two regions, where every possible position sum is included in the result.
   * When one of the regions is just a vector, this corresponds to a translation of the other region.
   */
  def ++ (v: Region[X, ?]): Region[X, ?] = ProductRegion(this, v)
  
  /**
   * Take the cartesian product one region and the negation of the other.
   * This yields a new region describing the set of all differences between
   * positions in the first and second region.
   */
  def -- (v: Region[X, ?]): Region[X, ?] = ProductRegion(this, -v)
  
  /** Negate all points in this region. */
  def unary_- : Region[X, T] = (0 until dim).foldLeft(this)(_.flip(_))
  
  /** Take the union of two regions, where a position is included in the result if it is present in either operand. */
  def | [S] (that: Region[X, S]): Region[X, (Option[T], Option[S])] = UnionRegion(this, that)
  
  /** Take the intersection of two regions, where a position is included in the result if it is present in both operands. */
  def & [S] (that: Region[X, S]): Region[X, (T, S)] = IntersectionRegion(this, that)
  
  /** Take the difference of two regions, where a position is included in the result if it is present in the first but not the second operand. */
  def \ [S] (that: Region[X, ?]): Region[X, T] = DifferenceRegion(this, that)
  
  /** Multiply every position in this region by a constant. */
  def * (x: X): Region[X, T] = map(_ * x)
  
  def map(f: Vec[X] => Vec[X]): Region[X, T] = MappedRegion(this, f)
  
  def filter(f: Vec[X] => Boolean): Region[X, T] = FilteredRegion(this, f)
  
  def paint[S](f: (Vec[X], => T) => S): Region[X, S] = PaintedRegion(this, f)
  def paint[S](f: Vec[X] => S): Region[X, S] = paint((v, _) => f(v))
  
  def paintPartial[X2 >: T, S](f: Vec[X] => Option[S]): Region[X, X2 | S] =
    paint((v, x) => f(v).getOrElse(x))
  
  def paintSolid[S](s: S): Region[X, S] = paint((_, _) => s)
  
  def paintLabels[S](f: T => S): Region[X, S] = paint((_, x) => f(x))
  
  def replace[S](s: (Region[X, ?], S)*): Region[X, T | S] =
    s.foldLeft[Region[X, T | S]](this):
      case (k, (v, s)) =>
        val vs = v.positions.toSet
        k.paintPartial(x => Option.when(vs.contains(x))(s))
        
  def zipWithPosition: Region[X, (Vec[X], T)] = paint((v, x) => (v, x))
  
  def neighbours(v: Vec[X])(using M: EnumerableMetric[X]): Region[X, T] =
    (this & M.neighbours(v)).paintLabels(_(0))
    
  def ball(v: Vec[X], rmax: Int, rmin: Int = 0)(using M: EnumerableMetric[X]): Region[X, T] =
    (this & M.ball(v, rmax, rmin)).paintLabels(_(0))
    
  lazy val posById: IndexedSeq[Vec[X]] = positions.toIndexedSeq
  lazy val indexOf: Map[Vec[X], Int] = posById.zipWithIndex.toMap
  
object Region:
  
  def apply[X : Ring : Ordering](v: Vec[X]*): Shape[X] = SetRegion(v)
  def apply[X : Ring : Ordering](v: Iterable[Vec[X]]): Shape[X] = SetRegion(v)
  
  def apply[X : Ring : Ordering, T](v: (Vec[X], T)*): Region[X, T] =
    val colours = Map(v*)
    Region(v.map(_(0))).paint(colours.apply)
   
  
  /** A rectangular region of space with the given dimensions. */
  def box(size: VecI): Shape[Int] = BoxRegion(size)
  /** A rectangular region of space with the given dimensions. */
  def box(size: Int*): Shape[Int] = box(VecI(size*))
  /** A rectangular region of space with the given extremal corners. */
  def box(start: VecI, end: VecI): RegionI =
    BoxRegion(end - start + VecI.one(Math.max(start.dim, end.dim))) ++ start
    
  /** A 2D-region consisting of a single row. */
  def row(width: Int): RegionI = box(width, 1)
  /** A 2D-region consisting of a single column. */
  def col(height: Int): RegionI = box(1, height)
  
  /** An empty 2D-region of a given width, used for creating spaces in stacked regions. */
  def hspace[X : Ordering](width: X)(using R: Ring[X]): Region[X, Nothing] =
    Region.empty(width, R.additiveIdentity)
  /** An empty 2D-region of a given height, used for creating spaces in stacked regions. */
  def vspace[X : Ordering](height: X)(using R: Ring[X]): Region[X, Nothing] =
    Region.empty(R.additiveIdentity, height)
  
  /**
   * A region built from aligning sub-regions with each other.
   *
   * @param align The rule for aligning the sub-regions.
   * @param regions The sub-regions to combine.
   */
  def join[T](align: Align)(regions: Region[Int, T]*): Region[Int, T] =
    regions.foldLeft(Region.empty[Int])(_.join(_, align))
    
  /**
   * A region built from stacking sub-regions end-to-end.
   *
   * @param dim The dimension along which the sub-regions are stacked.
   * @param regions The sub-regions to stack.
   */
  def stack[T](dim: Int)(regions: Region[Int, T]*): Region[Int, T] =
    join(Align.stack(dim))(regions*)
    
  /** A region built from stacking 2D-sub-regions horizontally. */
  def hstack[T](regions: Region[Int, T]*): Region[Int, T] = stack(0)(regions*)
  /** A region built from stacking 2D-sub-regions vertically. */
  def vstack[T](regions: Region[Int, T]*): Region[Int, T] = stack(1)(regions.reverse*)
  
  def empty[X : Ring : Ordering](size: Vec[X]): Region[X, Nothing] = EmptyRegion(size)
  def empty[X : Ring : Ordering](size: X*): Region[X, Nothing] = empty(Vec(size*))
  def empty[X : Ring : Ordering]: Region[X, Nothing] = EmptyRegion(Vec.empty)
  
  /** A region containing all space. */
  def infinite[V : Ring : Ordering]: Shape[V] = InfiniteRegion()
  
  /**
   *
   * @tparam X The discrete field over which the vectors contained therein are defined (usually Int).
   */
  trait Shape[X : Ring : Ordering] extends Region[X, Unit]:
    def label(v: Vec[X]): Option[Unit] = Option.when(contains(v))(())
    
  /**
   * A region containing an empty region.
   * @param size The dimensions of the bounding box of this region,
   * used to create spacing when stacking regions side-by-side.
   */
  private case class EmptyRegion[X: Ring: Ordering] (
    override val size: Vec[X],
  ) extends Region[X, Nothing]:
    
    def positions: Iterator[Nothing] = Iterator.empty
    def contains(v: Vec[X]): false = false
    override val start: Vec[X] = Vec.empty
    
    def label(v: Vec[X]): None.type = None
  
  /** A region containing all space. */
  private case class InfiniteRegion [X : Ordering] () (using R: Ring[X]) extends Shape[X]:
    
    def positions: Iterator[Vec[X]] = Iterator.iterate(Vec.empty)(_ + Vec(R.additiveIdentity))
    def contains(v: Vec[X]): Boolean = true
    override val start: Vec[X] = Vec.empty
    override val size: Vec[X] = Vec.empty
    
  /**
   * A region containing a (hyper)-rectangular subset of space.
   * Begins at the origin and extends into the positive orthant.
   * @param size The dimensions of the box.
   */
  private case class BoxRegion (
    override val size: VecI,
  ) extends Shape[Int]:
    
    def positions: Iterator[VecI] =
      
      @tailrec
      def rec(d: VecI, prefix: Iterator[VecI] = Iterator(VecI.empty)): Iterator[VecI] =
        d.iterator.toSeq match
          case x +: v =>
            rec(Vec(v), for u <- prefix; i <- Iterator.range(0, x) yield u :+ i)
          case _ => prefix
      
      rec(size)
    
    def contains(v: VecI): Boolean = inBounds(v)
    override val start: VecI = VecI.zero(size.dim)
    
  private case class WindowRegion [+T] (
    base: Region[Int, T],
    override val start: VecI,
    override val end: VecI,
  ) extends Region[Int, T]:
    
    lazy val box: RegionI = Region.box(start, end)
    
    def positions: Iterator[VecI] = box.positions.filter(base.contains)
    def contains(v: VecI): Boolean = start <= v && v <= end && base.contains(v)
    def label(v: VecI): Option[T] = Option.when(start <= v && v <= end)(base.label(v)).flatten
  
  /** A rotated projection of some base region. */
  private case class RotatedRegion [X : Ordering, +T] (
    base: Region[X, T],
    from: Int,
    to: Int,
  ) (using R: Ring[X]) extends Region[X, T]:
    
    def positions: Iterator[Vec[X]] = base.positions.map(_.rotate(from, to))
    def contains(v: Vec[X]): Boolean = base.contains(v.rotate(to, from))
    
    override val start: Vec[X] = base.start
      .update(from, -base.end(to))
      .update(to, base.start(from))
    override val size: Vec[X] = base.size.rotate(to, from).flip(from)
    
    // Optimisation: Not required for correctness!
    override def rotate(from: Int, to: Int): Region[X, T] =
      if from == this.to && to == this.from then base
      else super.rotate(from, to)
      
    def label(v: Vec[X]): Option[T] =
      base.label(v.rotate(to, from))
  
  /** A flipped projection of some base region. */
  private case class FlippedRegion [X : Ordering, +T] (
    base: Region[X, T],
    axis: Int,
  ) (using R: Ring[X]) extends Region[X, T]:
    
    def positions: Iterator[Vec[X]] = base.positions.map(_.flip(axis))
    def contains(v: Vec[X]): Boolean = base.contains(v.flip(axis))
    
    override val start: Vec[X] = base.start.update(axis, -base.end(axis))
    override val size: Vec[X] = base.size
    
    // Optimisation: Not required for correctness!
    override def flip(axis: Int): Region[X, T] =
      if axis == this.axis then base
      else super.flip(axis)
      
    def label(v: Vec[X]): Option[T] =
      base.label(v.flip(axis))
  
  /**
   *
   * @param leftRegion
   * @param rightRegion
   * @param align
   * @param ring$V$0
   * @tparam X The discrete field over which the vectors contained therein are defined (usually Int).
   * @tparam L The type of the label assigned to each position, if there is one.
   * @tparam R
   */
  private case class JoinedRegion [L, R] (
    leftRegion: Region[Int, L],
    rightRegion: Region[Int, R],
    align: Align,
  ) extends Region[Int, L | R]:
    
    private val offset: VecI =
      align.relativeOffset(leftRegion.size, rightRegion.size) + (leftRegion.start - rightRegion.start)
    
    def positions: Iterator[VecI] =
      (leftRegion.positions ++ rightRegion.positions.map(_ + offset)).distinct
      
    def contains(v: VecI): Boolean =
      leftRegion.contains(v) || rightRegion.contains(v - offset)
      
    override val start: VecI =
      leftRegion.start.zip(rightRegion.start + offset)(Math.min)
      
    override val end: VecI =
      leftRegion.end.zip(rightRegion.end + offset)(Math.max)
      
    def label(v: VecI): Option[L | R] =
      rightRegion.label(v - offset).orElse(leftRegion.label(v))
  
  /**
   *
   * @param leftRegion
   * @param rightRegion
   * @param ring$V$0
   * @tparam X The discrete field over which the vectors contained therein are defined (usually Int).
   * @tparam L The type of the label assigned to each position, if there is one.
   * @tparam R
   */
  private case class ProductRegion [X : Ring : Ordering, L, R] (
    leftRegion: Region[X, L],
    rightRegion: Region[X, R],
  ) extends Region[X, (L, R)]:
    
    def positions: Iterator[Vec[X]] =
      (for
        pos1 <- leftRegion.positions
        pos2 <- rightRegion.positions
      yield pos1 + pos2
      ).distinct
      
    def contains(v: Vec[X]): Boolean =
      leftRegion.positions.exists(u => rightRegion.contains(v - u))
      
    override val start: Vec[X] = leftRegion.start + rightRegion.start
    override val end: Vec[X] = leftRegion.end + rightRegion.end
    
    def label(v: Vec[X]): Option[(L, R)] = null
  
  /** The union of two base regions, contains all positions from either. */
  private case class UnionRegion [X : Ring, L, R] (
    leftRegion: Region[X, L],
    rightRegion: Region[X, R],
  ) (using O: Ordering[X]) extends Region[X, (Option[L], Option[R])]:
  
    def positions: Iterator[Vec[X]] =
      (leftRegion.positions ++ rightRegion.positions).distinct
      
    def contains(v: Vec[X]): Boolean =
      leftRegion.contains(v) || rightRegion.contains(v)
    
    override val start: Vec[X] = leftRegion.start.zip(rightRegion.start)(O.min)
    override val end: Vec[X] = leftRegion.end.zip(rightRegion.end)(O.max)
    
    def label(v: Vec[X]): Option[(Option[L], Option[R])] =
      Option.when(contains(v))(leftRegion.label(v), rightRegion.label(v))
  
  /** The intersection of two base regions, contains only positions that are in both. */
  private case class IntersectionRegion [X : Ring, L, R] (
    leftRegion: Region[X, L],
    rightRegion: Region[X, R],
  ) (using O: Ordering[X]) extends Region[X, (L, R)]:
    
    def positions: Iterator[Vec[X]] =
      leftRegion.positions.filter(rightRegion.contains)
        .zip(rightRegion.positions.filter(leftRegion.contains))
        .map((v, _) => v)
      
    def contains(v: Vec[X]): Boolean =
      leftRegion.contains(v) && rightRegion.contains(v)
    
    override val start: Vec[X] = leftRegion.start.zip(rightRegion.start)(O.max)
    override val end: Vec[X] = leftRegion.end.zip(rightRegion.end)(O.min)
    
    def label(v: Vec[X]): Option[(L, R)] =
      leftRegion.label(v).zip(rightRegion.label(v))
  
  /** The difference of two base regions, contains only positions in left and not in right. */
  private case class DifferenceRegion [X : Ring : Ordering, T] (
    leftRegion: Region[X, T],
    rightRegion: Region[X, ?],
  ) extends Region[X, T]:
    
    def positions: Iterator[Vec[X]] =
      leftRegion.positions.filterNot(rightRegion.contains)
    
    def contains(v: Vec[X]): Boolean =
      leftRegion.contains(v) && !rightRegion.contains(v)
    
    override lazy val start: Vec[X] = leftRegion.start
    override lazy val end: Vec[X] = leftRegion.end
    
    def label(v: Vec[X]): Option[T] =
      Option.when(!rightRegion.contains(v))(leftRegion.label(v)).flatten
  
  private case class FilteredRegion [X : Ring : Ordering, T] (
    base: Region[X, T],
    f: Vec[X] => Boolean
  ) extends Region[X, T]:
    
    def positions: Iterator[Vec[X]] =
      base.positions.filter(f)
    
    def contains(x: Vec[X]): Boolean =
      f(x) && base.contains(x)
    
    override val start: Vec[X] = base.start
    override val end: Vec[X] = base.end
    
    def label(v: Vec[X]): Option[T] =
      Option.when(contains(v))(base.label(v)).flatten
  
  private case class MappedRegion [V : Ring, X] (
    base: Region[V, X],
    f: Vec[V] => Vec[V],
  ) (using O : Ordering[V]) extends Region[V, X]:
    
    def positions: Iterator[Vec[V]] =
      base.positions.map(f).distinct
      
    def contains(v: Vec[V]): Boolean =
      positions.contains(v)
      
    def label(v: Vec[V]): Option[X] =
      base.positions.find(f(_) == v).flatMap(base.label)
    
    override lazy val start: Vec[V] = positions.reduce(_.zip(_)(O.min))
    override lazy val end: Vec[V] = positions.reduce(_.zip(_)(O.max))
    
  private case class PaintedRegion [V : Ring : Ordering, X, Y] (
    base: Region[V, X],
    f: (Vec[V], => X) => Y,
  ) extends Region[V, Y]:
    
    export base.{positions, contains}
    override val start: Vec[V] = base.start
    override val end: Vec[V] = base.end
    
    def label(v: Vec[V]): Option[Y] =
      base.label(v).map(l => f(v - base.start, l))
    
    override def paint[Z](g: (Vec[V], => Y) => Z): Region[V, Z] =
      base.paint((v, x) => g(v - base.start, f(v - base.start, x)))
      
  private case class SetRegion [V : Ring] (
    private val pos: Iterable[Vec[V]],
  ) (using O : Ordering[V]) extends Shape[V]:
    
    def positions: Iterator[Vec[V]] = pos.iterator
    private lazy val posSet = pos.toSet
    export posSet.contains
    
    override val dim = pos.headOption.map(_.dim).getOrElse(0)
    override lazy val start = positions.reduceOption(_.zip(_)(O.min)).getOrElse(Vec.empty)
    override lazy val end = positions.reduceOption(_.zip(_)(O.max)).getOrElse(Vec.empty)
  
  /** A collection of integer vectors combined to describe a region in space. */
  type RegionI = Region[Int, ?]
  
  extension [T] (region: Region[Int, T])
    
    /**
     * A view of this region which is restricted only to a given bounding box.
     *
     * @param start The smallest corner of the bounding box.
     * @param end The largest corner of the bounding box.
     */
    def window(start: VecI, end: VecI): Region[Int, T] =
      WindowRegion(region, start, end)
    
    /**
     * A view of this region which is restricted to a given segment along some axis.
     *
     * @param axis The axis whose values to bound.
     * @param min The lower threshold for the value on this axis.
     * @param max The upper threshold for the value on this axis.
     */
    def slice(axis: Int, min: Int, max: Int): Region[Int, T] =
      window(region.start.update(axis, min), region.end.update(axis, max))
    
    /** A view of this 2D-region which is restricted to a single row. */
    def row(i: Int): Region[Int, T] = slice(1, i, i)
    /** A view of this 2D-region which is restricted to a single column. */
    def col(j: Int): Region[Int, T] = slice(0, j, j)
    
    def join[S](that: Region[Int, S], align: Align): Region[Int, S | T] =
      JoinedRegion(region, that, align)
    
    /**
     * Create a ray in this direction starting from the given source.
     * @param source The starting point(s) of the ray.
     * @param inclusive Whether the source itself is included (default=false).
     */
    def rayFrom(source: RegionI, inclusive: Boolean = false): Ray =
      Ray.from(source, region, inclusive)
    
    /**
     * Create a ray in this direction starting from the current position,
     * with the current position excluded from the ray.
     * @param source The starting point(s) of the ray.
     */
    def rayFromHere(using source: VecI): Ray =
      Ray.from(source, region)
    
    /** Translate this region so that it becomes relative to the current position. */
    def fromHere(using source: VecI): RegionI = region ++ source