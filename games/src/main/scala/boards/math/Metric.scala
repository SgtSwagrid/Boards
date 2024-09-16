package boards.math

import Algebra.{*, given}
import Vec.{*, given}
import boards.math.kernel.Kernel
import Kernel.{*, given}

trait Metric[X: Ring]:
  def norm(p: Vec[X]): X
  inline final def dist(p: Vec[X], q: Vec[X]): X = norm(q - p)

object Metric:
    
  object Euclidean extends Metric[Float]:
    inline def norm(p: Vec[Float]): Float = Math.sqrt(p dot p).toFloat
  
  trait EnumerableMetric[X: Ring] extends Metric[X]:
    def ball(p: Vec[X], rmax: Int, rmin: Int = 0): Kernel[Unit]
    inline final def adjacent(p: Vec[X], q: Vec[X]): Boolean = dist(p, q) == 1
    inline final def neighbours(p: Vec[X]): Kernel[Unit] = ball(p, 1, 1)
  
  object Manhattan extends EnumerableMetric[Int]:
    
    inline def norm(p: VecI): Int =
      p.map(_.abs).sum
    
    override def ball(p: VecI, rmax: Int, rmin: Int = 0): Kernel[Unit] =
      DiamondKernel(p.dim, rmax, rmin).translate(p)
  
  object EuclideanSquared extends EnumerableMetric[Int]:
    
    inline def norm(p: VecI): Int = p dot p
    
    def ball(p: VecI, rmax: Int, rmin: Int = 0): Kernel[Unit] =
      CircleKernel(p.dim, rmax, rmin).translate(p)
  
  object Chebyshev extends EnumerableMetric[Int]:
    
    inline def norm(p: VecI): Int =
      p.map(_.abs).foldLeft(0)(Math.max)
    
    def ball(p: VecI, rmax: Int, rmin: Int = 0): Kernel[Unit] =
      SquareKernel(p.dim, rmax, rmin).translate(p)
  
  private trait NormKernel extends Shape:
    
    val rmax: Int
    val rmin: Int
    
    val offset: VecI = -Vec.one(dim) * rmax
    val size: VecI = Vec.one(dim) * (2 * rmax + 1)
  
  private case class DiamondKernel (
    override val dim: Int,
    rmax: Int,
    rmin: Int = 0
  ) extends NormKernel:
    
    def positions: Iterator[VecI] =
      
      def rec(dim: Int, rmax: Int, rmin: Int): Iterator[VecI] =
        dim match
          case 0 => Iterator(VecI.zero)
          case dim =>
            for
              r <- Iterator.range(if dim == 1 then rmin else 0, rmax + 1)
              b <- rec(dim - 1, rmax - r, Math.max(rmin - r, 0))
              sign <- Iterator(-1, 1)
            yield (r * sign) +: b
      
      rec(dim, rmax, rmin)
    
    def contains(v: VecI): Boolean =
      val length = v.norm(using Metric.Manhattan)
      rmin <= length && length <= rmax
  
  private case class CircleKernel (
    override val dim: Int,
    rmax: Int,
    rmin: Int = 0
  ) extends NormKernel:
    
    def positions: Iterator[VecI] =
      SquareKernel(dim, rmax, rmin).positions.filter(contains)
    
    def contains(v: VecI): Boolean =
      val length = v.norm(using Metric.EuclideanSquared)
      rmin * rmin <= length && length <= rmax * rmax
  
  private case class SquareKernel (
    override val dim: Int,
    rmax: Int,
    rmin: Int = 0
  ) extends NormKernel:
    
    def positions: Iterator[VecI] =
      
      def rec(dim: Int, rmax: Int, rmin: Int): Iterator[VecI] =
        dim match
          case 0 => Iterator(VecI.zero)
          case dim =>
            val ends = for
              b <- rec(dim - 1, rmax, 0)
              r <- Iterator.range(rmin, rmax + 1)
              sign <- Seq(-1, 1)
            yield (r * sign) +: b
            val middle = for
              b <- rec(dim - 1, rmax, rmin) if !b.isEmpty
              r <- Iterator.range(1 - rmin, rmin)
            yield r +: b
            ends ++ middle
      
      rec(dim, rmax, rmin)
    
    def contains(v: VecI): Boolean =
      val length = v.norm(using Metric.Chebyshev)
      rmin <= length && length <= rmax