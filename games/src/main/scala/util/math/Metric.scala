package util.math

import util.math.Vec.{*, given}
import util.math.kernel.Kernel.{*, given}
import util.math.kernel.Kernel

trait Metric:
  
  def norm(p: VecI): Int
  def ball(p: VecI, rmax: Int, rmin: Int = 0): Kernel[Unit]
  
  inline final def dist(p: VecI, q: VecI): Int = norm(q - p)
  inline final def adjacent(p: VecI, q: VecI): Boolean = dist(p, q) == 1
  inline final def neighbours(p: VecI): Kernel[Unit] = ball(p, 1, 1)

object Metric:
  
  case object Manhattan extends Metric:
    
    inline override def norm(p: VecI): Int =
      p.map(_.abs).sum
    
    override def ball(p: VecI, rmax: Int, rmin: Int = 0): Kernel[Unit] =
      DiamondKernel(p.dim, rmax, rmin).translate(p)
  
  case object EuclideanSquared extends Metric:
    
    inline override def norm(p: VecI): Int =
      p.map(n => n * n).sum
    
    override def ball(p: VecI, rmax: Int, rmin: Int = 0): Kernel[Unit] =
      CircleKernel(p.dim, rmax, rmin).translate(p)
  
  case object Chebyshev extends Metric:
    
    inline override def norm(p: VecI): Int =
      p.map(_.abs).foldLeft(0)(Math.max)
    
    override def ball(p: VecI, rmax: Int, rmin: Int = 0): Kernel[Unit] =
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