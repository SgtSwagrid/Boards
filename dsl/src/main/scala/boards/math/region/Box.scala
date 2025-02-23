package boards.math.region

import boards.math.region.BoundingBox.{BoundingBoxI, UBoundingBoxI}
import boards.math.region.Vec.{HasUVecI, HasVecI, VecI}
import boards.math.Unbounded.UInt
import boards.util.extensions.CollectionOps.cartesianProduct
import boards.math.Conversions.{*, given}
import boards.math.Algebra.{*, given}

import scala.annotation.tailrec

/** An axis-aligned rectangular region over the integers. */
case class Box private (
  boundingBox: UBoundingBoxI,
) extends Region[Int]:
  
  lazy val positions =
    boundingBox.toBounded.intervals.map: interval =>
      interval.start to interval.end
    .cartesianProduct.map(Vec.apply)
  
  def contains (v: HasVecI) = inBounds(v)
  
  override def window(boundingBox: UBoundingBoxI): Box =
    Box.fromBounds((this.boundingBox & boundingBox).toBounded)
  
  override val area = size.product
  override def toString = boundingBox.toString

object Box:
  
  /** Create a possibly-unbounded box region from a bounding box. */
  def unbounded (boundingBox: UBoundingBoxI): Box = new Box(boundingBox)
  /** Create a possibly-unbounded box region of a given size in the positive orthant. */
  def unbounded (size: HasUVecI): Box = Box.unbounded(BoundingBox.fromOrigin(size))
  /** Create a possibly-unbounded box region of a given size in the positive orthant. */
  def unbounded (size: UInt*): Box = Box.unbounded(Vec(size*))
  /** Create a possibly-unbounded box region with the given min and max corners. */
  def unbounded (start: HasUVecI, end: HasUVecI): Box = Box.unbounded(BoundingBox(start, end))
  
  /** Create a bounded box region from a bounding box. */
  def fromBounds (boundingBox: BoundingBoxI): Box = Box.unbounded(boundingBox.toUnbounded)
  /** Create a bounded box region of a given size in the positive orthant. */
  def apply (size: HasVecI): Box = Box.fromBounds(BoundingBox.fromOrigin(size))
  /** Create a bounded box region of a given size in the positive orthant. */
  def apply (size: Int*): Box = Box(VecI(size*))
  /** Create a bounded box region with the given min and max corners. */
  def apply (start: HasVecI, end: HasVecI): Box = Box.fromBounds(BoundingBox(start, end))
  
  /** A 2D-region consisting of a single row. */
  def row (width: Int): Box = Box(width, 1)
  /** A 2D-region consisting of a single column. */
  def col (height: Int): Box = Box(1, height)