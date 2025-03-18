package boards.math.vector

import boards.math.algebra.Algebra.Ring
import boards.math.vector.Bounds.BoundsI
import boards.math.vector.Region.RegionI
import boards.math.vector.Vec.VecI
import boards.math.Conversions.{*, given}

/** A metric in some vector space for describing distance.
 *
   * @tparam X The field over which this `Metric` is defined.
 */
trait Metric [X: Ring]:
  /** The length of the given `Vec`. */
  def norm (p: Vec[X]): X
  /** The distance between 2 `Vec`s `p`, `q`. Equivalent to `norm(q - p)`. */
  inline final def dist (p: Vec[X], q: Vec[X]): X = norm(q - p)

object Metric:
  
  /** The L2 norm for real-valued `Vec`s,
    * where distance is defined as the square root of the sum of the squares of the component-wise differences.
    */
  object Euclidean extends Metric[Float]:
    inline def norm (p: Vec[Float]): Float = Math.sqrt(p dot p).toFloat
  
  /** A metric in some vector space for describing distance.
    * Also has the ability to enumerate the neighbourhood of a `Vec`.
    * @tparam X The field over which this `EnumerableMetric` is defined.
    */
  trait EnumerableMetric [X: Ring] extends Metric[X]:
    /** Enumerate all `Vec`s which are between a distance of `rmin` and `rmax` from the given `Vec`. */
    def ball (p: Vec[X], rmax: Int, rmin: Int = 0): Region[X]
    /** Determine whether two `Vec`s are adjacent to one another. */
    inline final def adjacent (p: Vec[X], q: Vec[X]): Boolean = dist(p, q) == 1
    /** Enumerate the neighbourhood of a `Vec`. */
    inline final def neighbours (p: Vec[X]): Region[X] = ball(p, 1, 1)
  
  /** The L1 norm (a.k.a. Taxicab/Manhattan norm) for integer-valued `Vec`s,
    * where distance is defined as the sum of the absolute values of the component-wise differences.
    *
    * In particular, this describes the number of moves you must make to cover this distance
    * if you can only move one space orthogonally at a time.
    */
  object Manhattan extends EnumerableMetric[Int]:
    
    inline def norm (p: VecI): Int =
      p.position.map(_.abs).sum
    
    override def ball (p: VecI, rmax: Int, rmin: Int = 0): RegionI =
      DiamondRegion(p.position.dim, rmax, rmin) + p
  
  /** The square of the L2 norm for integer-valued `Vec`s,
    * where distance is defined as the sum of the squares of the component-wise differences.
    *
    * Note that this is not technically a norm.
    */
  object EuclideanSquared extends EnumerableMetric[Int]:
    
    inline def norm (p: VecI): Int = p.position dot p.position
    
    def ball (p: VecI, rmax: Int, rmin: Int = 0): RegionI =
      CircleRegion(p.position.dim, rmax, rmin) + p
  
  /** The L∞ norm (a.k.a. Chebyshev/King norm) for integer-valued `Vec`s,
    * where distance is defined as the largest component-wise difference.
    *
    * In particular, this describes the number of moves a chess king must make
    * to cover this distance.
    */
  object Chebyshev extends EnumerableMetric[Int]:
    
    inline def norm (p: VecI): Int =
      p.position.map(_.abs).foldLeft(0)(Math.max)
    
    def ball (p: VecI, rmax: Int, rmin: Int = 0): RegionI =
      SquareRegion(p.position.dim, rmax, rmin) + p

  object Hex extends EnumerableMetric[Int]:

    inline def norm (p: VecI): Int =
      ???

    def ball (p: VecI, rmax: Int, rmin: Int = 0): RegionI =
      HexRegion(rmax, rmin) + p
  
  /** A region used to enumerate the positions in a neighbourhood. */
  trait NormRegion extends Region[Int]:
    
    val rmax: Int
    val rmin: Int
    
    val bounds: BoundsI =
      Bounds.between(-Vec.one[Int](dim) * rmax, Vec.one[Int](dim) * rmax)
  
  /** A region comprised of an origin-centred diamond ring. */
  case class DiamondRegion (
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
              sign <- if r == 0 then LazyList(1) else LazyList(-1, 1)
            yield (r * sign) +: b
      
      rec(dim, rmax, rmin)
    
    def contains (v: VecI): Boolean =
      val length = Metric.Manhattan.norm(v)
      rmin <= length && length <= rmax
  
  /** A region comprised of an origin-centred circular ring. */
  case class CircleRegion (
    override val dim: Int,
    rmax: Int,
    rmin: Int = 0
  ) extends NormRegion:
    
    lazy val positions: LazyList[VecI] =
      SquareRegion(dim, rmax, rmin).positions.filter(contains)
    
    def contains (v: VecI): Boolean =
      val length = Metric.EuclideanSquared.norm(v)
      rmin * rmin <= length && length <= rmax * rmax
  
  /** A region comprised of an origin-centred square ring. */
  case class SquareRegion (
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
              sign <- if r == 0 then LazyList(1) else LazyList(-1, 1)
            yield (r * sign) +: b
            val middle = for
              b <- rec(dim - 1, rmax, rmin) if !b.isEmpty
              r <- LazyList.range(1 - rmin, rmin)
            yield r +: b
            ends ++ middle
      
      rec(dim, rmax, rmin)
    
    def contains (v: VecI): Boolean =
      val length = Metric.Chebyshev.norm(v)
      rmin <= length && length <= rmax

  case class HexRegion (
    rmin: Int,
    rmax: Int,
  ) extends NormRegion:

    override val dim: Int = 2

    lazy val positions: LazyList[VecI] =
      LazyList(VecI(0, 1), VecI(0, -1), VecI(1, 0), VecI(-1, 0), VecI(-1, 1), VecI(1, -1))

    def contains (v: VecI): Boolean =
      v.x.abs <= 1 && v.y.abs <= 1 && v.x != v.y