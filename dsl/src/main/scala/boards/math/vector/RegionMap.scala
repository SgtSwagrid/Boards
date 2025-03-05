package boards.math.vector

import boards.math.vector.RegionMap.*
import boards.math.algebra.Algebra.Numeric
import boards.math.algebra.Unbounded.Finite
import boards.math.algebra.Bijection.AffineBijection
import boards.math.ops.RegionOps
import boards.math.vector.Region.HasRegion

/** A partial mapping from discrete vectors in some space to arbitrary labels.
 *
  * @tparam X The discrete field over which the vectors contained herein are defined (usually [[Int]]).
  * @tparam A The type of the labels associated with each vector.
  */
trait RegionMap[@specialized X: Numeric, +A] extends
  Region[X],
  RegionOps[X, [Y] =>> RegionMap[Y, A]]:
  
  /** The region over which this map is defined. */
  protected val keys: Region[X]
  protected def getLabel (v: Vec[X]): A
  
  /** Get the label associated with this position, if the position is in bounds. */
  def label (v: Vec[X]): Option[A] =
    Option.when(contains(v))(getLabel(v.position))
    
  /** Get the label associated with this position, throwing an exception if the position is out of bounds. */
  def apply (v: Vec[X]): A = label(v).get
  
  /** Get the set of all labels for positions in this region. */
  def labels: LazyList[A] = positions.flatMap(label)
  /** Get the set of all position-label pairs in this region. */
  def entries: LazyList[(Vec[X], A)] = zipWithPosition.labels
  /** Convert this structure to a standard [[HashMap]]. */
  def toMap: Map[Vec[X], A] = Map.from(entries)

  lazy val positions: LazyList[Vec[X]] = keys.positions
  override def contains (v: Vec[X]): Boolean = keys.contains(v)
  
  lazy val bounds: Bounds[X] = keys.bounds
  
  override def window (window: Bounds[X]): RegionMap[X, A] =
    WindowRegionMap(this, window)
  
  override def mapAffine [Y: Numeric] (f: AffineBijection[X, Y]): RegionMap[Y, A] =
    TransformRegionMap(this, f)
    
  def | [B] (that: RegionMap[X, B]): RegionMap[X, A | B] =
    (this.keys | that.keys).withLabels(v => this.label(v).orElse(that.label(v)).get)
    
  override def & (that: Region[X]): RegionMap[X, A] =
    IntersectionRegionMap(this, that)
  
  override def filter (f: Vec[X] => Boolean): RegionMap[X, A] =
    this & keys.filter(f)
    
  /** Apply a function to each label. */
  def mapLabels [B] (f: A => B): RegionMap[X, B] =
    PaintedRegionMap(this, (_, a) => f(a))
    
  /** Filter positions based on their label. */
  def filterLabels (f: A => Boolean): RegionMap[X, A] =
    filter(v => f(apply(v)))
    
  /** Replace some of the labels with those in the updated region. */
  def update [B] (updated: RegionMap[X, B]): RegionMap[X, A | B] =
    zipWithPosition.mapLabels: (position, label) =>
      if updated.contains(position) then updated(position) else label
    
  /** Combine two maps, keeping only positions which appear in both regions.
    * @see zipAll
    */
  def zip [B] (that: RegionMap[X, B]): RegionMap[X, (A, B)] =
    (this & that).zipWithPosition.mapLabels((v, a) => (a, that(v)))
  
  /** Combine two maps, keeping all positions which appear in either region.
    * @see zip
    */
  def zipAll [B] (that: RegionMap[X, B]): RegionMap[X, (Option[A], Option[B])] =
    (this | that).withLabels(v => (this.label(v), that.label(v)))
    
  /** Tag each label with the position to which it is attached. */
  def zipWithPosition: RegionMap[X, (Vec[X], A)] =
    PaintedRegionMap(this, (v, a) => (v, a))
    
  override def toString =
    entries.map((k, v) => s"$k -> $v").mkString("{", ", ", "}")
  
object RegionMap:
  
  type RegionMapI [+A] = RegionMap[Int, A]
  type RegionMapL [+A] = RegionMap[Long, A]
  
  trait HasRegionMap [X: Numeric, +A] extends HasRegion[X]:
    def region: RegionMap[X, A]
    
  given [X, A]: Conversion[HasRegionMap[X, A], RegionMap[X, A]] = _.region
  
  def apply [X: Numeric, A] (entries: (Vec[X], A)*): RegionMap[X, A] =
    from(entries.toMap)
  
  def from [X: Numeric, A] (entries: Map[Vec[X], A]): RegionMap[X, A] =
    Region.from(entries.map((v, _) => v)).withLabels(entries.apply)
    
  def from [X: Numeric, A] (entries: Seq[(Vec[X], A)]): RegionMap[X, A] =
    RegionMap(entries*)
    
  def from [X: Numeric] (region: Region[X]): RegionMap[X, Unit] =
    LiftedRegionMap(region)
  
  def empty [X: Numeric]: RegionMap[X, Nothing] =
    EmptyRegionMap()
  
  private[math] case class EmptyRegionMap [X: Numeric] () extends RegionMap[X, Nothing]:
    
    val keys: Region[X] = Region.empty
    def getLabel (v: Vec[X]): Nothing = throw new IllegalStateException
  
  private[math] case class LiftedRegionMap [X: Numeric] (
    override val keys: Region[X],
  ) extends RegionMap[X, Unit]:
    
    def getLabel (v: Vec[X]): Unit = ()
  
  private[math] case class PaintedRegionMap [X: Numeric, A, B] (
    base: RegionMap[X, A],
    f: (Vec[X], A) => B
  ) extends RegionMap[X, B]:
    
    val keys = base.keys
    def getLabel (v: Vec[X]): B = f(v, base.getLabel(v))
    
  private[math] case class WindowRegionMap [X: Numeric, A] (
    base: RegionMap[X, A],
    window: Bounds[X],
  ) extends RegionMap[X, A]:
    
    val keys = base.keys.window(window)
    def getLabel (v: Vec[X]): A = base.getLabel(v)
  
  private[math] case class TransformRegionMap [X: Numeric, Y: Numeric, A] (
    base: RegionMap[X, A],
    f: AffineBijection[X, Y],
  ) extends RegionMap[Y, A]:
    
    val keys = base.keys.mapAffine(f)
    def getLabel (v: Vec[Y]): A = base.getLabel(f.inverse(v.toUnbounded).toFinite)
  
  private[math] case class IntersectionRegionMap [X: Numeric, A] (
    base: RegionMap[X, A],
    subset: Region[X],
  ) extends RegionMap[X, A]:
    
    val keys = base.keys & subset
    def getLabel (v: Vec[X]): A = base.getLabel(v)