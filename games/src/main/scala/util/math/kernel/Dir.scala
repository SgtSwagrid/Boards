package util.math.kernel

import util.math.Algebra.{*, given}
import util.math.Number.*
import util.math.Vec.VecI
import util.math.{Metric, Vec}
import util.math.kernel.Kernel
import util.math.kernel.Kernel.given
import util.math.kernel.Kernel.Shape

import scala.annotation.targetName

class Dir(val positions: Int => Kernel[?]):
  
  def toKernel (using
                origin: VecI,
                domain: Kernel[?] = Kernel.infinite
  ): Kernel[?] =
    from(origin)
  
  def from
    (origin: VecI)
    (using domain: Kernel[?] = Kernel.infinite)
  : Kernel[?] =
    positions(origin.dim).translate(origin)
  
  def ray (using
           start: VecI = VecI.zero,
           domain: Kernel[?] = Kernel.infinite
  ): Ray =
    Ray.from(start, this)
  
  def rayFrom (
    start: VecI = VecI.zero,
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
  def + (pos: VecI): Kernel[?] = from(pos)
  
  @targetName("multiply")
  def * (x: Int): Dir = new Dir(dim => Kernel(positions(dim).positions.toSeq.map(_ * x)))

object Dir:
  
  def apply(dirs: VecI*): Dir = new Dir(_ => Kernel(dirs*))
  def apply(f: Int => Kernel[?]): Dir = new Dir(f)
  
  def orthogonal: Dir = Dir: dim =>
    Metric.Manhattan.neighbours(VecI.zero(dim))
  
  def diagonal: Dir =
    octagonal - orthogonal
  
  def octagonal: Dir = Dir: dim =>
    Metric.Chebyshev.neighbours(VecI.zero(dim))
    
  def axis(axis: Int): Dir = Dir: dim =>
    Kernel(-VecI.axis(axis, dim), VecI.axis(axis, dim))
  
  def horizontal: Dir = Dir(Vec(-1, 0), Vec(1, 0))
  def vertical: Dir = Dir(Vec(0, -1), Vec(0, 1))
  
  def up: Dir = Dir(VecI.up)
  def down: Dir = Dir(VecI.down)
  def left: Dir = Dir(VecI.left)
  def right: Dir = Dir(VecI.right)
  
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
      
  def between(from: VecI, to: VecI): Dir =
    Dir(from.directionTo(to))
    
  given Conversion[VecI, Dir] with
    def apply(pos: VecI): Dir = Dir(pos)
    
  given (using VecI): Conversion[Dir, Kernel[?]] with
    def apply(dir: Dir): Kernel[?] = dir.toKernel
    
  extension (pos: VecI)
    @targetName("translate")
    def + (dir: Dir): Kernel[?] = dir + pos