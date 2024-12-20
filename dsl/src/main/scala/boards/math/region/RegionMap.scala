package boards.math.region

import boards.imports.math.{*, given}
import boards.math.Algebra.Ring
import Align.*
import boards.math.Bijection.{<=>, AffineBijection}
import Vec.{HasVec, HasVecI, UVec}
import boards.math.WithInfinity
import boards.math.region.BoundingBox.UBoundingBox
import boards.math.region.Region.*
import boards.math.region.RegionMap.*
import boards.math.region.RegionOps.{FilterOps, IntersectionOps, TransformOps}

trait RegionMap[@specialized X : Ring : Ordering, +A] extends
  Region[X],
  RegionOps[X, [Y] =>> RegionMap[Y, A]]:
  
  protected val keys: Region[X]
  protected def getLabel(v: Vec[X]): A
  
  def label(v: HasVec[X]): Option[A] =
    Option.when(region.contains(v))(getLabel(v.position))
    
  def apply(v: HasVec[X]): A = label(v).get
    
  def labels: LazyList[A] = positions.flatMap(label)
  def entries: LazyList[(Vec[X], A)] = zipWithPosition.labels
  def toMap: Map[Vec[X], A] = Map.from(entries)

  lazy val positions: LazyList[Vec[X]] = keys.positions
  override def contains(v: HasVec[X]): Boolean = keys.contains(v)
  
  lazy val boundingBox: UBoundingBox[X] = keys.boundingBox
  
  override def window(window: UBoundingBox[X]): RegionMap[X, A] =
    WindowRegionMap(this, window)
  
  override def map [Y : Ring : Ordering] (f: AffineBijection[X, Y]): RegionMap[Y, A] =
    TransformRegionMap(this, f)
    
  override def & (that: HasRegion[X]): RegionMap[X, A] =
    IntersectionRegionMap(this, that.region)
  
  override def filter(f: Vec[X] => Boolean): RegionMap[X, A] =
    this & keys.filter(f)
    
  def mapLabels[B](f: A => B): RegionMap[X, B] =
    PaintedRegionMap(this, (_, a) => f(a))
    
  def filterLabels(f: A => Boolean): RegionMap[X, A] =
    filter(v => f(apply(v)))
    
  def zip[B](that: RegionMap[X, B]): RegionMap[X, (A, B)] =
    (this & that).zipWithPosition.mapLabels((v, a) => (a, that(v)))
  
  def zipAll[B](that: RegionMap[X, B]): RegionMap[X, (Option[A], Option[B])] =
    (this | that).withLabels(v => (this.label(v), that.label(v)))
    
  def zipWithPosition: RegionMap[X, (Vec[X], A)] =
    PaintedRegionMap(this, (v, a) => (v, a))
    
  override def toString =
    entries.map((k, v) => s"$k -> $v").mkString("{", ", ", "}")
  
object RegionMap:
  
  type RegionMapI[+A] = RegionMap[Int, A]
  type RegionMapL[+A] = RegionMap[Long, A]
  
  def apply[X: Ring: Ordering, A](entries: (Vec[X], A)*): RegionMap[X, A] =
    from(entries.toMap)
  
  def from[X: Ring: Ordering, A](entries: Map[Vec[X], A]): RegionMap[X, A] =
    Region.from(entries.map((v, _) => v)).withLabels(entries.apply)
    
  def from[X: Ring: Ordering, A](entries: Seq[(Vec[X], A)]): RegionMap[X, A] =
    RegionMap(entries*)
    
  def from[X: Ring: Ordering](region: HasRegion[X]): RegionMap[X, Unit] =
    LiftedRegionMap(region.region)
  
  def empty[X: Ring: Ordering]: RegionMap[X, Nothing] =
    EmptyRegionMap()
  
  private[math] case class EmptyRegionMap[X: Ring: Ordering] () extends RegionMap[X, Nothing]:
    val keys: Region[X] = Region.empty
    def getLabel(v: Vec[X]): Nothing = throw new IllegalStateException
  
  private[math] case class LiftedRegionMap [X: Ring: Ordering] (
    override val keys: Region[X],
  ) extends RegionMap[X, Unit]:
    def getLabel(v: Vec[X]): Unit = ()
  
  private[math] case class PaintedRegionMap [X: Ring: Ordering, A, B] (
    base: RegionMap[X, A],
    f: (Vec[X], A) => B
  ) extends RegionMap[X, B]:
    
    val keys = base.keys
    def getLabel(v: Vec[X]): B = f(v, base.getLabel(v))
    
  private[math] case class WindowRegionMap [X: Ring: Ordering, A] (
    base: RegionMap[X, A],
    window: UBoundingBox[X],
  ) extends RegionMap[X, A]:
    
    val keys = base.keys.window(window)
    def getLabel(v: Vec[X]): A = base.getLabel(v)
  
  private[math] case class TransformRegionMap [X: Ring: Ordering, Y: Ring: Ordering, A] (
    base: RegionMap[X, A],
    f: AffineBijection[X, Y],
  ) extends RegionMap[Y, A]:
    
    val keys = base.keys.map(f)
    def getLabel(v: Vec[Y]): A = base.getLabel(f.finite.inverse(v))
  
  private[math] case class IntersectionRegionMap [X: Ring: Ordering, A] (
    base: RegionMap[X, A],
    subset: Region[X],
  ) extends RegionMap[X, A]:
    
    val keys = base.keys & subset
    def getLabel(v: Vec[X]): A = base.getLabel(v)
  
  