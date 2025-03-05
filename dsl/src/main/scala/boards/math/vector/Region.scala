package boards.math.vector

import boards.math.algebra.Algebra.{*, given}
import boards.math.algebra.Bijection.AffineBijection
import boards.math.algebra.Unbounded.given
import boards.math.Conversions.{*, given}
import Region.*
import boards.math.vector.Vec
import boards.math.vector.Vec.{VecI, UVec}
import boards.math.algebra.Unbounded.{Finite, UInt}
import boards.math.ops.RegionOps
import boards.math.ops.TransformOps.AffineFunctor
import boards.math.ops.AffineOps.Affine
import boards.math.ops.MinkowskiOps.Difference

import scala.annotation.{tailrec, targetName}

/** A collection of vectors combined to describe a region in arbitrary-dimensional Euclidean space.
  * @tparam X The discrete field over which the vectors contained herein are defined (usually [[Int]]).
  */
trait Region[@specialized X: Numeric] extends
  Difference[Region[X]],
  RegionOps[X, Region]:
  
  /** An iterator over all positions in this region.
    *
    * Note: If the region is infinite, this list will not end.
    */
  lazy val positions: LazyList[Vec[X]]
  
  /** Whether the given position is contained in this region.
    * @see [[inBounds]].
    */
  def contains (v: Vec[X]): Boolean
  
  /** Whether the given position is contained in this region.
    * @see [[inBounds]].
    */
  def contains (v: X*): Boolean = contains(Vec(v*))
  
  /** Determines whether the given region is a subset of this one. */
  def containsRegion (region: Region[X]): Boolean =
    region.positions.forall(this.contains)
  
  /** Determines whether this region is the empty region. */
  def isEmpty: Boolean = positions.isEmpty
  /** Determines whether this region contains at least one position. */
  def nonEmpty: Boolean = positions.nonEmpty
  
  /** The number of distinct positions in this region. */
  def area: UInt = Finite(positions.size)
  
  /** If this region contains exactly one position, convert it to a [[Vec]]. */
  def asVec: Option[Vec[X]] = if positions.sizeIs == 1 then positions.headOption else None
  
  /** A vector containing only the multiplicative identity. */
  protected final def zero: Vec[X] = Vec.zero(dim)
  /** A vector containing only the additive identity. */
  protected final def one: Vec[X] = Vec.one(dim)
  
  def window (window: Bounds[X]): Region[X] =
    WindowRegion(this, window)
  
  def mapAffine [Y: Numeric] (f: AffineBijection[X, Y]): Region[Y] =
    TransformRegion(this, f)
  
  /**
   * Take the cartesian product of two regions, where every possible position sum is included in the result.
   * When one of the regions is just a vector, this corresponds to a translation of the other region.
   */
  def minkowskiSum (v: Region[X]): Region[X] =
    MinkowskiRegion(this, v)
  
  /** Negate all points in this region. */
  def negate: Region[X] =
    (0 until dim).foldLeft(this)(_.flip(_))
  
  /** Take the union of two regions, where a position is included in the result if it is present in either operand. */
  final def | (that: Region[X]): Region[X] =
    UnionRegion(this, that)
  
  def & (that: Region[X]): Region[X] =
    IntersectionRegion(this, that)
  
  def ^ (that: Region[X]): Region[X] =
    (this \ that) | (that \ this)
  
  def filter (f: Vec[X] => Boolean): Region[X] = FilteredRegion(this, f)
  
  def join (using X =:= Int) (that: RegionI, align: Align): RegionI =
    val offset = align.relativeOffset (
      asRegionI.bounds.toBoundsF,
      that.bounds.toBoundsF
    ).toVecI
    asRegionI | (that + offset)
  
  /**
   * Create a ray in this direction starting from the given source.
   *
   * @param source The starting point(s) of the ray.
   * @param inclusive Whether the source itself is included (default=false).
   */
  def rayFrom (using X =:= Int) (source: RegionI, inclusive: Boolean = false): Ray =
    Ray.from(source, asRegionI, inclusive)
  
  /**
   * Create a ray in this direction starting from the current position,
   * with the current position excluded from the ray.
   *
   * @param source The starting point(s) of the ray.
   */
  def rayFromHere (using source: VecI) (using X =:= Int): Ray =
    Ray.from(source, asRegionI)
  
  /** Convert this [[Region]] to a [[RegionMap]] with [[Unit]] labels. */
  def toRegionMap: RegionMap[X, Unit] = RegionMap.from(this)
  
  /** Attach the same constant label to each position. */
  def fill [A] (a: A): RegionMap[X, A] =
    toRegionMap.mapLabels(_ => a)
    
  /** Attach a label to each position equal to the position itself. */
  def labelByPosition: RegionMap[X, Vec[X]] =
    toRegionMap.zipWithPosition.mapLabels((v, _) => v)
    
  /** Attach a label to each position. */
  def withLabels [A] (f: Vec[X] => A): RegionMap[X, A] =
    toRegionMap.zipWithPosition.mapLabels((v, _) => f(v))
    
  protected def asRegionI (using X =:= Int): RegionI =
    this.asInstanceOf[RegionI]
    
  lazy val posById: IndexedSeq[Vec[X]] = positions.toIndexedSeq
  lazy val indexOf: Map[Vec[X], Int] = posById.zipWithIndex.toMap
  
  override def equals (that: Any): Boolean = that match
    case that: Region[?] =>
      positions.toSet.equals(that.positions.toSet)
    case _ => false
    
  override def toString: String = positions.size match
    case 0 => "∅"
    case 1 => positions.head.toString
    case _ => positions.mkString("{", " | ", "}")
  
object Region:
  
  type RegionI = Region[Int]
  type RegionL = Region[Long]
  
  trait HasRegion [X: Numeric]:
    def region: Region[X]
  
  given [X]: Conversion[HasRegion[X], Region[X]] = _.region
  
  def apply [X: Numeric] (v: Vec[X]*): Region[X] = SetRegion(v.map(_.position))
  def from [X: Numeric] (v: Iterable[Vec[X]]): Region[X] = SetRegion(v.map(_.position))
  def point [X: Numeric] (v: Vec[X]): Region[X] = PointRegion(v.position)
  
  /** An empty 2D-region of a given width, used for creating spaces in stacked regions. */
  def hspace [X: Numeric as R] (width: X): Region[X] =
    Region.empty(width, R.additiveIdentity)
  /** An empty 2D-region of a given height, used for creating spaces in stacked regions. */
  def vspace [X: Numeric as R] (height: X): Region[X] =
    Region.empty(R.additiveIdentity, height)
  
  /** A region built from aligning sub-regions with each other.
    *
    * @param align The rule for aligning the sub-regions.
    * @param regions The sub-regions to combine.
    */
  def join (align: Align) (regions: RegionI*): RegionI =
    regions.foldLeft(Region.empty[Int]): (region, appendant) =>
      region.join(appendant, align)
    
  /** A region built from stacking sub-regions end-to-end.
    *
    * @param dim The dimension along which the sub-regions are stacked.
    * @param spacing The gap between each sub-region.
    * @param regions The sub-regions to stack.
    * @param reverse Whether to reverse the order of the stack.
    */
  def stack (
    dim: Int,
    spacing: Int = 0,
    reverse: Boolean = false
  ) (regions: RegionI*): RegionI =
    join(Align.stack(dim, spacing=spacing, reverse=reverse))(regions*)
    
  /** A region built from stacking 2D-sub-regions horizontally. */
  def hstack (regions: RegionI*): RegionI = stack(0)(regions*)
  
  /** A region built from stacking 2D-sub-regions vertically. */
  def vstack (regions: RegionI*): RegionI = stack(1)(regions.reverse*)
  
  def empty[X: Numeric](size: Vec[X]): Region[X] = EmptyRegion(size.position.toUnbounded)
  def empty[X: Numeric](size: X*): Region[X] = empty(Vec(size*))
  def empty[X: Numeric]: Region[X] = EmptyRegion(Vec.empty)
  
  /**
   * A region containing an empty region.
   * @param usize The dimensions of the bounding box of this region,
   * used to create spacing when stacking regions side-by-side.
   */
  private case class EmptyRegion [X: Numeric] (
    override val usize: UVec[X],
  ) extends Region[X]:
    
    lazy val positions: LazyList[Nothing] = LazyList.empty
    override def contains (v: Vec[X]): false = false
    val bounds = Bounds.empty
    override def toString = "∅"
    
    override def window (window: Bounds[X]): Region[X] = this
    
  private case class WindowRegion [X: Numeric] (
    base: Region[X],
    window: Bounds[X],
  ) extends Region[X]:
  
    lazy val positions = base.positions.filter(window.contains)
    def contains (v: Vec[X]): Boolean = window.contains(v) && base.contains(v)
    val bounds = base.bounds & window
  
  private case class TransformRegion [X: Numeric, Y: Numeric] (
    base: Region[X],
    f: AffineBijection[X, Y],
  ) extends Region[Y]:
    
    lazy val positions = base.positions.map(_.toUnbounded).map(f).map(_.toFinite[Y])
    def contains (v: Vec[Y]) = base.contains(f.inverse(v.toUnbounded).toFinite[X])
    val bounds = base.bounds.map(f)
    override def area = base.area
    
    override def window (window: Bounds[Y]): Region[Y] =
      base.window(window.map(f.inverse)).mapAffine(f)
  
  /**
   *
   * @param left
   * @param right
   * @param ring$V$0
   * @tparam X The discrete field over which the vectors contained therein are defined (usually Int).
   * @tparam L The type of the label assigned to each position, if there is one.
   * @tparam R
   */
  private case class MinkowskiRegion [X: Numeric] (
    left: Region[X],
    right: Region[X],
  ) extends Region[X]:
    
    lazy val positions =
      (for
        pos1 <- left.positions
        pos2 <- right.positions
      yield pos1 + pos2
      ).distinct
    
    def contains (v: Vec[X]) =
      inBounds(v) && left.positions.exists(u => right.contains(v - u))
      
    val bounds = left.bounds + right.bounds
    
    override def toString = s"$left + $right"
  
  /** The union of two base regions, contains all positions from either. */
  private case class UnionRegion [X: Numeric] (
    left: Region[X],
    right: Region[X],
  ) extends Region[X]:
  
    lazy val positions = (left.positions ++ right.positions).distinct
    def contains (v: Vec[X]) = left.contains(v) || right.contains(v)
    val bounds = left.bounds | right.bounds
    
    override lazy val area =
      left.area + right.area - (left & right).area
    
    override def window (window: Bounds[X]): Region[X] =
      left.window(window) | right.window(window)
      
    override def toString = s"$left | $right"
  
  /** The intersection of two base regions, contains only positions that are in both. */
  private case class IntersectionRegion [X: Numeric] (
    left: Region[X],
    right: Region[X],
  ) extends Region[X]:
    
    lazy val positions =
      left.window(right.bounds).positions.filter(right.contains)
        .zip(right.window(left.bounds).positions.filter(left.contains))
        .map((v, _) => v)
    
    def contains (v: Vec[X]) =
      left.contains(v) && right.contains(v)
    
    val bounds = left.bounds & right.bounds
    
    override def window (window: Bounds[X]): Region[X] =
      left.window(window) & right.window(window)
    
    override def toString = s"$left & $right"
  
  private case class FilteredRegion [X: Numeric] (
    base: Region[X],
    f: Vec[X] => Boolean
  ) extends Region[X]:
    
    lazy val positions = base.positions.filter(f)
    def contains (v: Vec[X]) = f(v.position) && base.contains(v.position)
    val bounds = base.bounds
    
    override def window (window: Bounds[X]): Region[X] =
      base.window(window).filter(f)
    
  private case class SetRegion [X: Numeric] (
    private val pos: Iterable[Vec[X]],
  ) extends Region[X]:
    
    lazy val positions: LazyList[Vec[X]] = LazyList.from(pos)
    private lazy val posSet = pos.toSet
    def contains (v: Vec[X]) = posSet.contains(v.position)
    lazy val bounds = Bounds.bounding(pos.toSeq*)
    
  private case class PointRegion [X: Numeric] (
    private val position: Vec[X],
  ) extends Region[X]:
    
    lazy val positions = LazyList(position)
    def contains (v: Vec[X]) = position == v.position
    val bounds = Bounds.point(position)
    override val dim = position.dim