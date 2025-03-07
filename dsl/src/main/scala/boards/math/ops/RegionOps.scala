package boards.math.ops

import boards.math.ops.RegionOps.*
import boards.math.ops.TransformOps.AffineFunctor
import boards.math.ops.AffineOps.Affine
import boards.math.algebra.Algebra.{*, given}
import boards.math.algebra.Bijection
import boards.math.algebra.Bijection.AffineBijection
import boards.math.Conversions.{*, given}
import boards.math.vector.{Bounds, Vec}
import boards.math.vector.Bounds.HasBounds
import boards.math.vector.Metric.EnumerableMetric
import boards.math.vector.Region
import boards.math.vector.Vec


import boards.util.extensions.CollectionOps.cartesianProduct
import RegionOps.*

/** Some operations which can be performed on regions, separated for F-bounded polymorphism.
  *
  * @tparam X The type of the discrete field over which the region is defined.
  * @tparam This The type of region which is produced by each operFation.
  */
private[math] trait RegionOps [X: Numeric, +This[Y] <: RegionOps[Y, This]] extends
  WindowOps[X, This],
  IntersectionOps[X, This],
  FilterOps[X, This],
  TransformOps[X, This]

private[math] object RegionOps:
  
  /** Operations based on taking a window of a region.
    * @tparam X The type of the discrete field over which the region is defined.
    * @tparam This The type of region which is produced by each operation.
    */
  trait WindowOps [X: Numeric as R, +This[Y] <: WindowOps[Y, This]]
    extends HasBounds[X]:
    
    /** A view of this region which is restricted only to a given bounding box. */
    def window (boundingBox: Bounds[X]): This[X]
    
    /** A view of this region which is restricted only to a given bounding box.
      *
      * @param start The smallest corner of the bounding box.
      * @param end The largest corner of the bounding box.
      */
    def window (start: Vec[X], end: Vec[X]): This[X] =
      window(Bounds.between(start, end))
    
    /** A view of this region which is restricted to a given segment along some axis.
      *
      * @param axis The axis whose values to bound.
      * @param min The lower threshold for the value on this axis.
      * @param max The upper threshold for the value on this axis.
      */
    def slice (axis: Int, min: X, max: X): This[X] =
      window(bounds.ustart.toFinite.update(axis, min), bounds.uend.toFinite.update(axis, max))

    /** The subset of the region corresponding to a facet of the bounding box.
      * @param axis The axis along which the desired facet is extremal.
      * @param end Whether to take the starting or ending facet.
      */
    def facet (axis: Int, end: Boolean = false): This[X] =
      val x = if end then bounds.uend(axis).toFinite else bounds.ustart(axis).toFinite
      slice(axis, x, x)
        
    /** The subset of the 2D region corresponding to the left edge of the bounding box. */
    def leftEdge: This[X] = facet(0, false)
    /** The subset of the 2D region corresponding to the right edge of the bounding box. */
    def rightEdge: This[X] = facet(0, true)
    /** The subset of the 2D region corresponding to the top edge of the bounding box. */
    def topEdge: This[X] = facet(1, true)
    /** The subset of the 2D region corresponding to the bottom edge of the bounding box. */
    def bottomEdge: This[X] = facet(1, false)
    
    /** The set of all non-overlapping windows of a given size which fit within this region.
      * The grid of windows is aligned to the smallest corner of the bounding box.
      *
      * @param size The dimensions of each window.
      */
    def windows (size: Vec[X]): List[This[X]] =
      size.position.components.zipWithIndex.map: (size, dim) =>
        Iterator.iterate(bounds.ustart(dim).toFinite)(_ + size).takeWhile(_ <= bounds.uend(dim).toFinite).toSeq
      .cartesianProduct.toList.map(Vec.apply).map(start => window(start, start + size))
    
    /** The set of all non-overlapping windows of a given size which fit within this region.
      * The grid of windows is aligned to the smallest corner of the bounding box.
      *
      * @param size The dimensions of each window.
      */
    def windows (size: X*): List[This[X]] =
      windows(Vec(size))
    
    /** The set of all non-overlapping slices of a given thickness along the given axis. */
    def slices (axis: Int, width: X): List[This[X]] =
      windows(bounds.usize.toFinite.update(axis, width))
    
    /** A view of this 2D-region which is restricted to a single row. */
    def row (i: X): This[X] = slice(1, i, i)
    
    /** A view of this 2D-region which is restricted to a single column. */
    def col (j: X): This[X] = slice(0, j, j)
    
    /** The set of all rows of this 2D-region. */
    def rows: List[This[X]] = slices(1, R.one)
    
    /** The set of all columns of this 2D-region. */
    def cols: List[This[X]] = slices(0, R.one)
  
  /** Operations based on taking the set intersection of two regions.
    *
    * @tparam X    The type of the discrete field over which the region is defined.
    * @tparam This The type of region which is produced by each operation.
    */
  trait IntersectionOps[X: Numeric, +This[Y] <: IntersectionOps[Y, This]]:
    
    /** Take the intersection between two regions. */
    def & (region: Region[X]): This[X]
    
    /** Get all neighbours of the given position within this region.
      * @param M The metric used to determine the neighbours.
      */
    def neighbours (v: Vec[X]) (using M: EnumerableMetric[X]): This[X] =
      this & M.neighbours(v)
    
    /** Get all other positions in the region whose distance to the centre lies in some range. */
    def ball (v: Vec[X], rmax: Int, rmin: Int = 0) (using M: EnumerableMetric[X]): This[X] =
      this & M.ball(v, rmax, rmin)
  
  /** Operations based on transforming a region by filtering positions.
    *
    * @tparam X    The type of the discrete field over which the region is defined.
    * @tparam This The type of region which is produced by each operation.
    */
  trait FilterOps [X: Numeric, +This[Y] <: FilterOps[Y, This]]:
    
    /** Keep only those positions which satisfy some predicate. */
    def filter (f: Vec[X] => Boolean): This[X]
  
    /** Discard those positions which satisfy some predicate. */
    def filterNot (f: Vec[X] => Boolean): This[X] = filter(v => !f(v))
    
    /** Take the set difference between two regions. */
    def \ (region: Region[X]): This[X] = filterNot(region.contains)
  
  /**
    * Operations based on transforming a region by affine bijection.
    *
    * @tparam X The type of the discrete field over which the region is defined.
    * @tparam This The type of region which is produced by each operation.
    */
  trait TransformOps [X: Numeric, +This[Y] <: TransformOps[Y, This]]
  extends AffineFunctor[X, This], Affine[X, This[X]]:
    
    /** Translate the region with the given offset. */
    def translate (offset: Vec[X]): This[X] = mapAffine(Bijection.Translate(offset.position))
    
    /** Flip the region over the given axis. */
    def flip (axis: Int): This[X] = mapAffine(Bijection.Flip(axis))
    
    /** Rotate the region from one axis to another. */
    def rotate (from: Int, to: Int): This[X] = mapAffine(Bijection.Rotate(from, to))
    
    def scale (factors: Vec[X]): This[X] = mapAffine(Bijection.Scale(factors))