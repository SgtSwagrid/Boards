package boards.math.vector

import boards.math.vector.Bounds.BoundsI
import boards.math.vector.Vec.{HasUVecI, HasVecI, VecI}
import boards.math.algebra.Unbounded.UInt
import boards.util.extensions.CollectionOps.cartesianProduct
import boards.math.Conversions.{*, given}
import boards.math.algebra.Algebra.{*, given}
import boards.math.vector.Region.RegionI

import scala.annotation.tailrec

/** An axis-aligned rectangular region over the integers. */
case class Box private (
  bounds: BoundsI,
) extends Region[Int]:
  
  lazy val positions =
    bounds.intervals.map(_.enumerate)
    .cartesianProduct.map(Vec.apply)
  
  def contains (v: VecI) = inBounds(v)
  
  override def window (bounds: BoundsI): Box =
    Box.fromBounds(this.bounds & bounds)
  
  override val area = usize.product
  override def toString = bounds.toString

object Box:
  
  /** Create a bounded box region from a bounding box. */
  def fromBounds (bounds: BoundsI): Box = new Box(bounds)
  /** Create a bounded box region of a given size in the positive orthant. */
  def apply (size: VecI): Box = Box.fromBounds(Bounds.fromOrigin(size - Vec.one[Int](size.dim)))
  /** Create a bounded box region of a given size in the positive orthant. */
  def apply (size: Int*): Box = Box(VecI(size*))
  /** Create a bounded box region with the given min and max corners. */
  def between (start: VecI, end: VecI): Box = Box.fromBounds(Bounds.between(start, end))
  
  /** A 2D-region consisting of a single row. */
  def row (width: Int): Box = Box(width, 1)
  def rows (widths: Int*): RegionI = Region.stackY(widths.map(Box.row)*)

  /** A 2D-region consisting of a single column. */
  def col (height: Int): Box = Box(1, height)
  def cols (heights: Int*): RegionI = Region.stackX(heights.map(Box.col)*)