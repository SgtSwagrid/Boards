package boards.math.region

import boards.math.WithInfinity.ExtendedInt
import boards.math.region.Region.RegionI
import boards.math.region.RegionOps.WindowOps
import boards.math.region.Vec.{HasUVecI, HasVec, HasVecI, UVecI, VecI}
import boards.math.region.BoundingBox.{BoundingBoxI, UBoundingBoxI}
import boards.util.extensions.CollectionOps.cartesianProduct

import scala.annotation.tailrec

case class Box private (
  boundingBox: UBoundingBoxI,
) extends Region[Int]:
  
  lazy val positions =
    boundingBox.asFinite[Int].intervals.map: interval =>
      interval.start to interval.end
    .cartesianProduct.map(Vec.apply)
  
  def contains(v: HasVecI) = inBounds(v)
  
  override def window(boundingBox: UBoundingBoxI): Box =
    Box.fromBounds((this.boundingBox & boundingBox).asFinite[Int])
  
  override val area = size.product
  override def toString = boundingBox.toString

object Box:
  
  def unbounded(boundingBox: UBoundingBoxI): Box = new Box(boundingBox)
  def unbounded(size: HasUVecI): Box = Box.unbounded(BoundingBox.fromOrigin(size))
  def unbounded(size: ExtendedInt*): Box = Box.unbounded(Vec(size *))
  def unbounded(start: HasUVecI, end: HasUVecI): Box = Box.unbounded(BoundingBox(start, end))
  
  def fromBounds(boundingBox: BoundingBoxI): Box = Box.unbounded(boundingBox.asUnbounded)
  def apply(size: HasVecI): Box = Box.fromBounds(BoundingBox.fromOrigin(size))
  def apply(size: Int*): Box = Box(VecI(size*))
  def apply(start: HasVecI, end: HasVecI): Box = Box.fromBounds(BoundingBox(start, end))
  
  /** A 2D-region consisting of a single row. */
  def row(width: Int): Box = Box(width, 1)
  /** A 2D-region consisting of a single column. */
  def col(height: Int): Box = Box(1, height)