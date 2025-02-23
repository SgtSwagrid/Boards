package boards.math.region

import boards.math.Algebra.Ring
import boards.math.region.BoundingBox.UBoundingBoxI
import boards.math.region.Region.RegionI
import boards.math.region.Vec.{HasVec, HasVecI, VecI}
import boards.math.Conversions.{*, given}

/** A metric in some vector space for describing distance.
 *
   * @tparam X The field over which this `Metric` is defined.
 */
trait Metric [X: Ring]:
  /** The length of the given `Vec`. */
  def norm (p: HasVec[X]): X
  /** The distance between 2 `Vec`s `p`, `q`. Equivalent to `norm(q - p)`. */
  inline final def dist (p: HasVec[X], q: HasVec[X]): X = norm(q.position - p.position)

object Metric:
  
  /** The L2 norm for real-valued `Vec`s,
    * where distance is defined as the square root of the sum of the squares of the component-wise differences.
    */
  object Euclidean extends Metric[Float]:
    inline def norm (p: HasVec[Float]): Float = Math.sqrt(p.position dot p.position).toFloat
  
  /** A metric in some vector space for describing distance.
    * Also has the ability to enumerate the neighbourhood of a `Vec`.
    * @tparam X The field over which this `EnumerableMetric` is defined.
    */
  trait EnumerableMetric [X: Ring] extends Metric[X]:
    /** Enumerate all `Vec`s which are between a distance of `rmin` and `rmax` from the given `Vec`. */
    def ball (p: HasVec[X], rmax: Int, rmin: Int = 0): Region[X]
    /** Determine whether two `Vec`s are adjacent to one another. */
    inline final def adjacent (p: HasVec[X], q: HasVec[X]): Boolean = dist(p, q) == 1
    /** Enumerate the neighbourhood of a `Vec`. */
    inline final def neighbours (p: HasVec[X]): Region[X] = ball(p, 1, 1)
  
  /** The L1 norm (a.k.a. Taxicab/Manhattan norm) for integer-valued `Vec`s,
    * where distance is defined as the sum of the absolute values of the component-wise differences.
    *
    * In particular, this describes the number of moves you must make to cover this distance
    * if you can only move one space orthogonally at a time.
    */
  object Manhattan extends EnumerableMetric[Int]:
    
    inline def norm (p: HasVecI): Int =
      p.position.map(_.abs).sum
    
    override def ball (p: HasVecI, rmax: Int, rmin: Int = 0): RegionI =
      DiamondRegion(p.position.dim, rmax, rmin) + p
  
  /** The square of the L2 norm for integer-valued `Vec`s,
    * where distance is defined as the sum of the squares of the component-wise differences.
    *
    * Note that this is not technically a norm.
    */
  object EuclideanSquared extends EnumerableMetric[Int]:
    
    inline def norm (p: HasVecI): Int = p.position dot p.position
    
    def ball (p: HasVecI, rmax: Int, rmin: Int = 0): RegionI =
      CircleRegion(p.position.dim, rmax, rmin) + p
  
  /** The L∞ norm (a.k.a. Chebyshev/King norm) for integer-valued `Vec`s,
    * where distance is defined as the largest component-wise difference.
    *
    * In particular, this describes the number of moves a chess king must make
    * to cover this distance.
    */
  object Chebyshev extends EnumerableMetric[Int]:
    
    inline def norm (p: HasVecI): Int =
      p.position.map(_.abs).foldLeft(0)(Math.max)
    
    def ball (p: HasVecI, rmax: Int, rmin: Int = 0): RegionI =
      SquareRegion(p.position.dim, rmax, rmin) + p
  
  /** A region used to enumerate the positions in a neighbourhood. */
  private trait NormRegion extends Region[Int]:
    
    val rmax: Int
    val rmin: Int
    
    val boundingBox: UBoundingBoxI =
      BoundingBox(-Vec.one(dim) * rmax, Vec.one(dim) * rmax).toUnbounded
  
  /** A region comprised of an origin-centred diamond ring. */
  private case class DiamondRegion (
    override val dim: Int,
    rmax: Int,
    rmin: Int = 0
  ) extends NormRegion:
    
    lazy val positions: LazyList[VecI] =
      
      def rec (dim: Int, rmax: Int, rmin: Int): LazyList[VecI] =
        dim match
          case 0 => LazyList(VecI.empty)
          case dim =>
            for
              r <- LazyList.range(if dim == 1 then rmin else 0, rmax + 1)
              b <- rec(dim - 1, rmax - r, Math.max(rmin - r, 0))
              sign <- LazyList(-1, 1)
            yield (r * sign) +: b
      
      rec(dim, rmax, rmin)
    
    def contains (v: HasVecI): Boolean =
      val length = Metric.Manhattan.norm(v)
      rmin <= length && length <= rmax
  
  /** A region comprised of an origin-centred circular ring. */
  private case class CircleRegion (
    override val dim: Int,
    rmax: Int,
    rmin: Int = 0
  ) extends NormRegion:
    
    lazy val positions: LazyList[VecI] =
      SquareRegion(dim, rmax, rmin).positions.filter(contains)
    
    def contains (v: HasVecI): Boolean =
      val length = Metric.EuclideanSquared.norm(v)
      rmin * rmin <= length && length <= rmax * rmax
  
  /** A region comprised of an origin-centred square ring. */
  private case class SquareRegion (
    override val dim: Int,
    rmax: Int,
    rmin: Int = 0
  ) extends NormRegion:
    
    lazy val positions: LazyList[VecI] =
      
      def rec (dim: Int, rmax: Int, rmin: Int): LazyList[VecI] =
        dim match
          case 0 => LazyList(VecI.empty)
          case dim =>
            val ends = for
              b <- rec(dim - 1, rmax, 0)
              r <- LazyList.range(rmin, rmax + 1)
              sign <- Seq(-1, 1)
            yield (r * sign) +: b
            val middle = for
              b <- rec(dim - 1, rmax, rmin) if !b.isEmpty
              r <- LazyList.range(1 - rmin, rmin)
            yield r +: b
            ends ++ middle
      
      rec(dim, rmax, rmin)
    
    def contains (v: HasVecI): Boolean =
      val length = Metric.Chebyshev.norm(v)
      rmin <= length && length <= rmax