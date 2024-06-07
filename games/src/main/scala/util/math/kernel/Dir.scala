package util.math.kernel

import util.math.Algebra.{*, given}
import util.math.Number.*
import util.math.Pos.{*, given}
import util.math.{Metric, Pos, Vec}
import util.math.kernel.Kernel
import util.math.kernel.Kernel.given
import util.math.kernel.Kernel.Shape

import scala.annotation.targetName

class Dir(val positions: Int => Kernel[?]):
  
  def toKernel (using
    origin: Pos,
    domain: Kernel[?] = Kernel.infinite
  ): Kernel[?] =
    from(origin)
  
  def from
    (origin: Pos)
    (using domain: Kernel[?] = Kernel.infinite)
  : Kernel[?] =
    positions(origin.dim).translate(origin)
  
  def ray (using
    start: Pos = Pos.zero,
    domain: Kernel[?] = Kernel.infinite
  ): Ray =
    Ray.from(start, this)
  
  def rayFrom (
    start: Pos = Pos.zero,
    inclusive: Boolean = false
  ) (
    using domain: Kernel[?] = Kernel.infinite
  ): Ray =
    Ray.from(start, this, inclusive)
    
  @targetName("union")
  def | (that: Dir): Dir = new Dir
    (dim => this.positions(dim) | that.positions(dim))
  
  @targetName("intersection")
  def & (that: Dir): Dir = new Dir
    (dim => this.positions(dim) & that.positions(dim))
    
  @targetName("difference")
  def - (that: Dir): Dir =
    new Dir(dim => this.positions(dim) - that.positions(dim))
    
  @targetName("translate")
  def + (pos: Pos): Kernel[?] = from(pos)
  
  @targetName("multiply")
  def * (x: Int): Dir = new Dir(dim => Kernel(positions(dim).positions.toSeq.map(_ * x)))

object Dir:
  
  def apply(dirs: Pos*): Dir = new Dir(_ => Kernel(dirs*))
  def apply(f: Int => Kernel[?]): Dir = new Dir(f)
  
  def orthogonal: Dir = Dir: dim =>
    Metric.Manhattan.neighbours(Pos.zero(dim))
  
  def diagonal: Dir =
    octagonal - orthogonal
  
  def octagonal: Dir = Dir: dim =>
    Metric.Chebyshev.neighbours(Pos.zero(dim))
    
  def axis(axis: Int): Dir = Dir: dim =>
    Kernel(-Pos.axis(axis, dim), Pos.axis(axis, dim))
  
  def horizontal: Dir = Dir(Vec(-1, 0), Vec(1, 0))
  def vertical: Dir = Dir(Vec(0, -1), Vec(0, 1))
  
  def up: Dir = Dir(Pos.up)
  def down: Dir = Dir(Pos.down)
  def left: Dir = Dir(Pos.left)
  def right: Dir = Dir(Pos.right)
  
  def diagonallyUp: Dir = Dir(Vec(-1, 1), Vec(1, 1))
  def diagonallyDown: Dir = Dir(Vec(-1, -1), Vec(1, -1))
  def diagonallyLeft: Dir = Dir(Vec(-1, -1), Vec(-1, 1))
  def diagonallyRight: Dir = Dir(Vec(1, -1), Vec(1, 1))
  
  def knight(steps: Int*): Dir =
    val dirs =
      for
        dir <- steps.permutations.toSeq
        sign <- Seq.fill(dir.length)(Seq(Seq(-1), Seq(1)))
          .reduce((A, B) => for (a <- A; b <- B) yield a ++ b)
      yield Vec(dir*) * Vec(sign*)
    Dir(dirs*)
      
  def between(from: Pos, to: Pos): Dir =
    Dir(from.directionTo(to))
    
  given Conversion[Pos, Dir] with
    def apply(pos: Pos): Dir = Dir(pos)
    
  given (using Pos): Conversion[Dir, Kernel[?]] with
    def apply(dir: Dir): Kernel[?] = dir.toKernel
    
  extension (pos: Pos)
    @targetName("translate")
    def + (dir: Dir): Kernel[?] = dir + pos