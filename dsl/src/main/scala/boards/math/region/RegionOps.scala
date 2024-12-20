package boards.math.region

import boards.math.WithInfinity.{*, given}
import boards.math.Algebra.{*, given}
import boards.math.Bijection.{<=>, AffineBijection, Flip, Rotate, Scale, Translate}
import boards.math.region.BoundingBox.{BoundingBoxI, HasBoundingBox, HasUBoundingBox, UBoundingBox, UBoundingBoxI}
import boards.math.region.Metric.EnumerableMetric
import boards.math.region.Region.HasRegion
import boards.math.region.Vec.{Dividable, HasVec, HasVecI, UVec, VecI}
import boards.util.extensions.CollectionOps.cartesianProduct
import RegionOps.*
import boards.dsl.pieces.PieceState.empty.region
import boards.math.{Bijection, WithInfinity}

private[math] trait RegionOps [X: Ring: Ordering, +This[Y] <: RegionOps[Y, This]] extends
  WindowOps[X, This],
  IntersectionOps[X, This],
  FilterOps[X, This],
  TransformOps[X, This]

private[math] object RegionOps:
  
  trait WindowOps [X: Ring as R: Ordering, +This[Y] <: WindowOps[Y, This]]
    extends HasUBoundingBox[X]:
    
    def window(boundingBox: UBoundingBox[X]): This[X]
    
    /**
     * A view of this region which is restricted only to a given bounding box.
     *
     * @param start The smallest corner of the bounding box.
     * @param end The largest corner of the bounding box.
     */
    /**
     * A view of this region which is restricted only to a given bounding box.
     *
     * @param start The smallest corner of the bounding box.
     * @param end The largest corner of the bounding box.
     */
    def window(start: HasVec[X], end: HasVec[X]): This[X] =
      window(BoundingBox(start.position.asUnbounded, end.position.asUnbounded))
    
    /**
     * A view of this region which is restricted to a given segment along some axis.
     *
     * @param axis The axis whose values to bound.
     * @param min The lower threshold for the value on this axis.
     * @param max The upper threshold for the value on this axis.
     */
    def slice(axis: Int, min: X, max: X): This[X] =
      boundingBox.asFinite match
        case BoundingBox.NonEmpty(start, end) =>
          window(start.update(axis, min), end.update(axis, max))
        case _ => window(BoundingBox.empty)
    
    def windows(size: HasVec[X]): List[This[X]] =
      boundingBox.asFinite match
        case BoundingBox.NonEmpty(start, end) =>
          size.position.components.zipWithIndex.map: (size, dim) =>
            Iterator.iterate(start(dim))(_ + size).takeWhile(_ <= end(dim)).toSeq
          .cartesianProduct.toList.map(Vec.apply).map(start => window(start, start + size))
        case BoundingBox.Empty() => List.empty
    
    def windows(size: X*): List[This[X]] =
      windows(Vec(size))
    
    def slices(axis: Int, width: X): List[This[X]] =
      windows(boundingBox.size.asFinite.update(axis, width))
    
    /** A view of this 2D-region which is restricted to a single row. */
    def row(i: X): This[X] = slice(1, i, i)
    
    /** A view of this 2D-region which is restricted to a single column. */
    def col(j: X): This[X] = slice(0, j, j)
    
    def rows: List[This[X]] = slices(1, R.multiplicativeIdentity)
    
    def cols: List[This[X]] = slices(0, R.multiplicativeIdentity)
  
  trait IntersectionOps[X: Ring: Ordering, +This[Y] <: IntersectionOps[Y, This]]:
    
    def & (region: HasRegion[X]): This[X]
    
    def neighbours(v: Vec[X])(using M: EnumerableMetric[X]): This[X] =
      this & M.neighbours(v)
    
    def ball(v: Vec[X], rmax: Int, rmin: Int = 0)(using M: EnumerableMetric[X]): This[X] =
      this & M.ball(v, rmax, rmin)
      
  trait FilterOps[X: Ring: Ordering, +This[Y] <: FilterOps[Y, This]]:
    
    def filter(f: Vec[X] => Boolean): This[X]
  
    def filterNot(f: Vec[X] => Boolean): This[X] = filter(v => !f(v))
    def \ (region: HasRegion[X]): This[X] = filterNot(region.region.contains)
  
  trait TransformOps[X: Ring: Ordering, +This[Y] <: TransformOps[Y, This]]:
    
    def map[Y: Ring: Ordering](f: AffineBijection[X, Y]): This[Y]
    
    def flip(axis: Int): This[X] = map(Flip(axis))
    def rotate(from: Int, to: Int): This[X] = map(Rotate(from, to))
    
    def translate(offset: HasVec[X]): This[X] = map(Translate(offset.position))
    def translate(offset: X*): This[X] = translate(Vec(offset))
    def fromHere(using offset: HasVec[X]): This[X] = translate(offset)
    
    def scale(factor: X)(using Field[X]): This[X] = map(Scale(factor))
    def * (x: X)(using Field[X]): This[X] = scale(x)