package boards.math.region

import boards.math.Algebra.{*, given}
import boards.math.Conversions.{*, given}
import Region.*
import boards.math.Bijection.AffineBijection
import boards.math.region.BoundingBox.UBoundingBox
import boards.math.region.Vec.{HasVec, HasVecI, UVec}
import boards.math.Unbounded.{Finite, UInt}

import scala.annotation.{tailrec, targetName}

/** A collection of vectors combined to describe a region in arbitrary-dimensional Euclidean space.
  * @tparam X The discrete field over which the vectors contained herein are defined (usually [[Int]]).
  */
trait Region[@specialized X: OrderedRing] extends
  HasRegion[X],
  RegionOps[X, Region]:
  
  val region: Region[X] = this
  
  /** An iterator over all positions in this region.
    *
    * Note: If the region is infinite, this list will not end.
    */
  lazy val positions: LazyList[Vec[X]]
  
  /** Whether the given position is contained in this region.
    * @see [[inBounds]].
    */
  def contains (v: HasVec[X]): Boolean
  
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
  
  def window (window: UBoundingBox[X]): Region[X] =
    WindowRegion(this, window)
  
  def map [Y: OrderedRing] (f: AffineBijection[X, Y]): Region[Y] =
    TransformRegion(this, f)
  
  /**
   * Take the cartesian product of two regions, where every possible position sum is included in the result.
   * When one of the regions is just a vector, this corresponds to a translation of the other region.
   */
  def + (v: HasRegion[X]): Region[X] =
    ProductRegion(this, v.region)
  
  /**
   * Take the cartesian product one region and the negation of the other.
   * This yields a new region describing the set of all differences between
   * positions in the first and second region.
   */
  def - (v: HasRegion[X]): Region[X] =
    ProductRegion(this, -v.region)
  
  /** Negate all points in this region. */
  def unary_- : Region[X] =
    (0 until dim).foldLeft(this)(_.flip(_))
  
  /** Take the union of two regions, where a position is included in the result if it is present in either operand. */
  final def | (that: HasRegion[X]): Region[X] =
    UnionRegion(this, that.region)
  
  def & (that: HasRegion[X]): Region[X] =
    IntersectionRegion(this, that.region)
  
  def ^ (that: HasRegion[X]): Region[X] =
    (this \ that.region) | (that.region \ this)
  
  def filter (f: Vec[X] => Boolean): Region[X] = FilteredRegion(this, f)
  
  def join (using X =:= Int) (that: HasRegionI, align: Align): RegionI =
    val offset = align.relativeOffset (
      asRegionI.boundingBox.toBounded.toBoundingBoxS,
      that.region.boundingBox.toBounded.toBoundingBoxS
    ).toVecI
    asRegionI | (that.region + offset)
  
  /**
   * Create a ray in this direction starting from the given source.
   *
   * @param source The starting point(s) of the ray.
   * @param inclusive Whether the source itself is included (default=false).
   */
  def rayFrom (using X =:= Int) (source: HasRegionI, inclusive: Boolean = false): Ray =
    Ray.from(source, asRegionI, inclusive)
  
  /**
   * Create a ray in this direction starting from the current position,
   * with the current position excluded from the ray.
   *
   * @param source The starting point(s) of the ray.
   */
  def rayFromHere (using source: HasVecI) (using X =:= Int): Ray =
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
    
  override def toString = positions.size match
    case 0 => "∅"
    case 1 => positions.head.toString
    case _ => positions.mkString("{", " | ", "}")
  
object Region:
  
  type RegionI = Region[Int]
  type RegionL = Region[Long]
  
  type HasRegionI = HasRegion[Int]
  type HasRegionL = HasRegion[Long]
  
  def apply [X: OrderedRing] (v: HasVec[X]*): Region[X] = SetRegion(v.map(_.position))
  def from [X: OrderedRing] (v: Iterable[HasVec[X]]): Region[X] = SetRegion(v.map(_.position))
  def point [X: OrderedRing] (v: HasVec[X]): Region[X] = PointRegion(v.position)
  
  /** An empty 2D-region of a given width, used for creating spaces in stacked regions. */
  def hspace [X: OrderedRing as R] (width: X): Region[X] =
    Region.empty(width, R.additiveIdentity)
  /** An empty 2D-region of a given height, used for creating spaces in stacked regions. */
  def vspace [X: OrderedRing as R] (height: X): Region[X] =
    Region.empty(R.additiveIdentity, height)
  
  /** A region built from aligning sub-regions with each other.
    *
    * @param align The rule for aligning the sub-regions.
    * @param regions The sub-regions to combine.
    */
  def join (align: Align) (regions: HasRegionI*): RegionI =
    regions.foldLeft(Region.empty[Int]): (region, appendant) =>
      region.join(appendant.region, align)
    
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
  ) (regions: HasRegionI*): RegionI =
    join(Align.stack(dim, spacing=spacing.toSurd, reverse=reverse))(regions*)
    
  /** A region built from stacking 2D-sub-regions horizontally. */
  def hstack (regions: HasRegionI*): RegionI = stack(0)(regions*)
  
  /** A region built from stacking 2D-sub-regions vertically. */
  def vstack (regions: HasRegionI*): RegionI = stack(1)(regions.reverse*)
  
  def empty[X: OrderedRing](size: HasVec[X]): Region[X] = EmptyRegion(size.position.toUnbounded)
  def empty[X: OrderedRing](size: X*): Region[X] = empty(Vec(size*))
  def empty[X: OrderedRing]: Region[X] = EmptyRegion(Vec.empty)
  
  /**
   * A region containing an empty region.
   * @param size The dimensions of the bounding box of this region,
   * used to create spacing when stacking regions side-by-side.
   */
  private case class EmptyRegion [X: OrderedRing] (
    override val size: UVec[X],
  ) extends Region[X]:
    
    lazy val positions: LazyList[Nothing] = LazyList.empty
    override def contains (v: HasVec[X]): false = false
    val boundingBox = BoundingBox.empty
    override def toString = "∅"
    
    override def window (window: UBoundingBox[X]): Region[X] = this
    
  private case class WindowRegion [X: OrderedRing] (
    base: Region[X],
    window: UBoundingBox[X],
  ) extends Region[X]:
  
    lazy val positions = base.positions.filter(window.inBounds)
    def contains (v: HasVec[X]): Boolean = window.inBounds(v) && base.contains(v)
    val boundingBox = base.boundingBox & window
  
  private case class TransformRegion [X: OrderedRing, Y: OrderedRing] (
    base: Region[X],
    f: AffineBijection[X, Y],
  ) extends Region[Y]:
    
    lazy val positions = base.positions.map(f.finite.apply)
    def contains (v: HasVec[Y]) = base.contains(f.finite.inverse(v.position))
    val boundingBox = base.boundingBox.bimap(f.infinite)
    override def area = base.area
    
    override def window (window: UBoundingBox[Y]): Region[Y] =
      base.window(window.bimap(f.infinite.invert)).map(f)
  
  /**
   *
   * @param left
   * @param right
   * @param ring$V$0
   * @tparam X The discrete field over which the vectors contained therein are defined (usually Int).
   * @tparam L The type of the label assigned to each position, if there is one.
   * @tparam R
   */
  private case class ProductRegion [X: OrderedRing] (
    left: Region[X],
    right: Region[X],
  ) extends Region[X]:
    
    lazy val positions =
      (for
        pos1 <- left.positions
        pos2 <- right.positions
      yield pos1 + pos2
      ).distinct
    
    def contains (v: HasVec[X]) =
      inBounds(v.position.toUnbounded) && left.positions.exists(u => right.contains(v.position - u))
      
    val boundingBox = left.boundingBox + right.boundingBox
    
    override def toString = s"$left + $right"
  
  /** The union of two base regions, contains all positions from either. */
  private case class UnionRegion [X: OrderedRing] (
    left: Region[X],
    right: Region[X],
  ) extends Region[X]:
  
    lazy val positions = (left.positions ++ right.positions).distinct
    def contains (v: HasVec[X]) = left.contains(v) || right.contains(v)
    val boundingBox = left.boundingBox | right.boundingBox
    
    override lazy val area =
      left.area + right.area - (left & right).area
    
    override def window (window: UBoundingBox[X]): Region[X] =
      left.window(window) | right.window(window)
      
    override def toString = s"$left | $right"
  
  /** The intersection of two base regions, contains only positions that are in both. */
  private case class IntersectionRegion [X: OrderedRing] (
    left: Region[X],
    right: Region[X],
  ) extends Region[X]:
    
    lazy val positions =
      left.window(right.boundingBox).positions.filter(right.contains)
        .zip(right.window(left.boundingBox).positions.filter(left.contains))
        .map((v, _) => v)
    
    def contains (v: HasVec[X]) =
      left.contains(v) && right.contains(v)
    
    val boundingBox = left.boundingBox & right.boundingBox
    
    override def window (window: UBoundingBox[X]): Region[X] =
      left.window(window) & right.window(window)
    
    override def toString = s"$left & $right"
  
  private case class FilteredRegion [X: OrderedRing] (
    base: Region[X],
    f: Vec[X] => Boolean
  ) extends Region[X]:
    
    lazy val positions = base.positions.filter(f)
    def contains (v: HasVec[X]) = f(v.position) && base.contains(v.position)
    val boundingBox = base.boundingBox
    
    override def window (window: UBoundingBox[X]): Region[X] =
      base.window(window).filter(f)
    
  private case class SetRegion [X: OrderedRing] (
    private val pos: Iterable[Vec[X]],
  ) extends Region[X]:
    
    lazy val positions: LazyList[Vec[X]] = LazyList.from(pos)
    private lazy val posSet = pos.toSet
    def contains (v: HasVec[X]) = posSet.contains(v.position)
    lazy val boundingBox = BoundingBox.bounding(pos.toSeq*).toUnbounded
    
  private case class PointRegion [X: OrderedRing] (
    private val position: Vec[X],
  ) extends Region[X]:
    
    lazy val positions = LazyList(position)
    def contains (v: HasVec[X]) = position == v.position
    val boundingBox = BoundingBox.point(position).toUnbounded
    override val dim = position.dim
  
  trait HasRegion [X: OrderedRing]:
    def region: Region[X]