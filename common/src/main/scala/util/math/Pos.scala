package util.math

import util.math.Number.gcd
import util.math.kernel.{Dir, Kernel, Ray}

import scala.annotation.targetName

object Pos:
  
  type Pos = Vec[Int]
  
  inline def zero: Pos = Vec.zero(0)
  inline def zero(d: Int = 0): Pos = Vec.zero(d)
  inline def one(d: Int = 0): Pos = Vec.one(d)
  inline def axis(n: Int, d: Int = 0): Pos = Vec.axis(n, d)
  
  inline def left: Pos = Vec.left
  inline def right: Pos = Vec.right
  inline def up: Pos = Vec.up
  inline def down: Pos = Vec.down
  
  inline def midpoint(v: Pos*): Pos =
    v.reduce(_ + _) / v.size
  
  extension (u: Pos)
    
    @targetName("divide")
    inline def / (d: Int): Pos = u.map(_ / d)
    
    inline def norm(using M: Metric): Int = M.norm(u)
    inline def ball(rmax: Int, rmin: Int = 0)(using M: Metric): Kernel[?] = M.ball(u, rmax, rmin)
    inline def dist(v: Pos)(using M: Metric): Int = M.dist(u, v)
    inline def adjacent(v: Pos)(using M: Metric): Boolean = M.adjacent(u, v)
    inline def neighbours(using M: Metric): Kernel[?] = M.neighbours(u)
    
    inline def neighbours(direction: Dir): Kernel[?] =
      direction.from(u)
    
    inline def directionTo(v: Pos): Pos = (v - u) / gcd((v - u).toSeq.map(_.abs)*)
    inline def midpoint(v: Pos): Pos = Pos.midpoint(u, v)
    
    inline def ray (
      direction: Dir,
      inclusive: Boolean = false
    ) (
      using domain: Kernel[?] = Kernel.infinite
    ): Ray =
      Ray.from(u, direction, inclusive)
      
    inline def rayTo(v: Pos): Ray = Ray.between(u, v)
    inline def rayUntil(v: Pos): Ray = Ray.between(u, v).drop(1) //wrong, drops from wrong end!
    
    //@targetName("add")
    //inline def + [X] (kernel: Kernel[X]): Kernel[X] = kernel.translate(u)
  
  given Conversion[0, Pos] with
    def apply(n: 0): Pos = Pos.zero