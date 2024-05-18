package util.math.kernel

import util.math.Algebra.{*, given}
import util.math.Vec.{*, given}
import util.math.Pos.{*, given}
import boards.graphics.Colour
import util.math.{Metric, Pos, Vec}

import scala.annotation.{tailrec, targetName}

trait Kernel[+X]:
  import Kernel.{*, given}
  
  def positions: Iterator[Pos]
  
  def contains(v: Pos): Boolean
  def contains(v: Int*): Boolean = contains(Vec(v*))
  
  def label(v: Pos): Option[X]
  def label(v: Int*): Option[X] = label(Vec(v*))
  
  def offset: Pos
  def size: Pos
  def extent: Pos = offset + size
  def dim: Int = size.dim
  
  def inBounds(v: Pos): Boolean = offset <= v && v < extent
    
  def window(offset: Pos, size: Pos): Kernel[X] =
    if offset <= this.offset && offset + size >= extent then this
    else IntersectionKernel(Kernel.box(size).translate(offset), this).map(_(1))
    
  def slice(dim: Int, min: Int, max: Int): Kernel[X] =
    window(offset.update(dim, min), size.update(dim, max - min + 1))
    
  def col(j: Int): Kernel[X] = slice(0, j, j)
  def row(i: Int): Kernel[X] = slice(1, i, i)
  
  def translate(v: Pos): Kernel[X] =
    if v.isZero then this else ShiftedKernel(this, v)
  def translate(v: Int*): Kernel[X] = translate(Vec(v*))
  
  def left(n: Int): Kernel[X] = translate(Vec.left[Int] * n)
  def right(n: Int): Kernel[X] = translate(Vec.right[Int] * n)
  def up(n: Int): Kernel[X] = translate(Vec.up[Int] * n)
  def down(n: Int): Kernel[X] = translate(Vec.down[Int] * n)
  
  def rotate(from: Int, to: Int): Kernel[X] =
    RotatedKernel(this, from, to)
  
  def flip(axis: Int): Kernel[X] =
    FlippedKernel(this, axis)
  
  def join[Y](that: Kernel[Y], align: Align): Kernel[X | Y] =
    JoinedKernel(this, that, align)
  
  @targetName("union")
  def | [Y] (that: Kernel[Y]): Kernel[(Option[X], Option[Y])] =
    UnionKernel(this, that)
  
  @targetName("intersection")
  def & [Y] (that: Kernel[Y]): Kernel[(X, Y)] =
    IntersectionKernel(this, that)
  
  @targetName("difference")
  def - [Y] (that: Kernel[Y]): Kernel[X] =
    DifferenceKernel(this, that)
    
  //@targetName("translate")
  //def + (v: Pos): Kernel[X] = translate(v)
  
  def filter(f: (Pos, X) => Boolean): Kernel[X] = FilteredKernel(this, f)
  def erode(f: Pos => Boolean): Kernel[X] = filter((v, _) => f(v))
  def filterLabels(f: X => Boolean): Kernel[X] = filter((_, x) => f(x))
  
  def paint[Y](f: (Pos, => X) => Y): Kernel[Y] = PaintedKernel(this, f)
  def paint[Y](f: Pos => Y): Kernel[Y] = paint((v, _) => f(v))
  
  def paintPartial[X2 >: X, Y](f: Pos => Option[Y]): Kernel[X2 | Y] =
    paint((v, x) => f(v).getOrElse(x))
  
  def paintSolid[Y](y: Y): Kernel[Y] = paint((_, _) => y)
  
  def map[Y](f: X => Y): Kernel[Y] = paint((_, x) => f(x))
  
  def replace[Y](y: (Kernel[?], Y)*): Kernel[X | Y] =
    y.foldLeft[Kernel[X | Y]](this):
      case (k, (v, y)) =>
        val vs = v.positions.toSet
        k.paintPartial(x => Option.when(vs.contains(x))(y))
  
  def neighbours(v: Pos)(using M: Metric): Kernel[X] =
    (this & M.neighbours(v)).map(_(0))
    
  def ball(v: Pos, rmax: Int, rmin: Int = 0)(using M: Metric): Kernel[X] =
    (this & M.ball(v, rmax, rmin)).map(_(0))
    
  lazy val posById: IndexedSeq[Pos] = positions.toIndexedSeq
  lazy val indexOf: Map[Pos, Int] = posById.zipWithIndex.toMap
    
  def draw: String =
    (offset.y until extent.y).map: y =>
      (offset.x until extent.x).map: x =>
        label(x, y) match
          case Some(colour: Colour) => colour("\u25A0")
          case Some(_) => "\u25A0"
          case None => " "
      .mkString
    .reverse.mkString("\n")
  
object Kernel:
  
  trait Shape extends Kernel[Unit]:
    def label(v: Pos): Option[Unit] = Option.when(contains(v))(())
  
  def apply(v: Pos*): Shape = SetKernel(v)
  def apply(v: Iterable[Pos]): Shape = SetKernel(v)
  
  def box(size: Pos): Shape = BoxKernel(size)
  def box(size: Int*): Shape = box(Vec(size *))
  def row(width: Int): Shape = box(width, 1)
  def col(height: Int): Shape = box(1, height)
  
  def hspace(width: Int): Kernel[Nothing] = Kernel.empty(width, 1)
  def vspace(height: Int): Kernel[Nothing] = Kernel.empty(1, height)
  
  def join[X](align: Align)(kernels: Kernel[X]*): Kernel[X] =
    kernels.foldLeft(Kernel.empty)(_.join(_, align))
  def stack[X](dim: Int)(kernels: Kernel[X]*): Kernel[X] =
    join(Align.stack(dim))(kernels *)
  def hstack[X](kernels: Kernel[X]*): Kernel[X] = stack(0)(kernels *)
  def vstack[X](kernels: Kernel[X]*): Kernel[X] = stack(1)(kernels.reverse *)
  
  def empty(size: Pos): Kernel[Nothing] = EmptyKernel(size)
  def empty(size: Int*): Kernel[Nothing] = empty(Vec(size *))
  def empty: Kernel[Nothing] = EmptyKernel()
  def infinite: Shape = InfiniteKernel
  
  private case object InfiniteKernel extends Shape:
    
    def positions: Iterator[Pos] = Iterator.iterate(Vec.zero[Int])(_ + Vec(1))
    def contains(v: Pos): Boolean = true
    val offset: Pos = Vec.zero[Int]
    val size: Pos = Vec.zero[Int]
  
  private case class EmptyKernel (
    size: Pos = 0
  ) extends Kernel[Nothing]:
    
    def positions: Iterator[Pos] = Iterator.empty
    def contains(v: Pos): Boolean = false
    def label(v: Pos): None.type = None
    val offset: Pos = 0
  
  private case class BoxKernel (
    size: Pos
  ) extends Shape:
    
    def positions: Iterator[Pos] =
      
      @tailrec
      def rec(d: Vec[Int], prefix: Iterator[Pos] = Iterator(0)): Iterator[Pos] =
        d match
          case x +: v =>
            rec(v, for u <- prefix; i <- Iterator.range(0, x) yield u :+ i)
          case _ => prefix
      
      rec(size)
    
    def contains(v: Pos): Boolean = 0 <= v && v < size
    
    val offset: Pos = 0
  
  private case class RotatedKernel[+X] (
    base: Kernel[X],
    from: Int,
    to: Int
  ) extends Kernel[X]:
    
    def positions: Iterator[Pos] =
      base.positions.map(_.rotate(from, to))
    
    def contains(v: Pos): Boolean =
      base.contains(v.rotate(to, from))
    
    def label(v: Pos): Option[X] =
      base.label(v.rotate(to, from))
    
    val offset: Pos = base.offset
      .update(from, 1 - base.extent(to))
      .update(to, base.offset(from))
    
    val size: Pos = base.size.rotate(to, from).flip(to)
    
    override def rotate(from: Int, to: Int): Kernel[X] =
      if from == this.to && to == this.from then base
      else super.rotate(from, to)
    
  private case class FlippedKernel[+X] (
    base: Kernel[X],
    axis: Int
  ) extends Kernel[X]:
    
    def positions: Iterator[Pos] =
      base.positions.map(_.flip(axis))
    
    def contains(v: Pos): Boolean =
      base.contains(v.flip(axis))
    
    def label(v: Pos): Option[X] =
      base.label(v.flip(axis))
      
    val offset: Pos = base.offset.update(axis, 1 - base.extent(axis))
    
    export base.size
    
    override def flip(axis: Int): Kernel[X] =
      if axis == this.axis then base
      else super.flip(axis)
      
  private case class JoinedKernel[X, Y] (
    left: Kernel[X],
    right: Kernel[Y],
    align: Align
  ) extends Kernel[X | Y]:
    
    private val right_offset: Pos =
      align.relativeOffset(left.size, right.size) + (left.offset - right.offset)
    
    def positions: Iterator[Pos] =
      lazy val right_positions =
        val left_positions = left.positions.toSet
        right.positions.map(_ + right_offset).filterNot(left_positions.contains)
      left.positions ++ right_positions
      
    def contains(v: Pos): Boolean =
      left.contains(v) || right.contains(v - right_offset)
      
    def label(v: Pos): Option[X | Y] =
      right.label(v - right_offset).orElse(left.label(v))
      
    val offset: Pos =
      left.offset.zip(right.offset + right_offset)(Math.min)
      
    val size: Pos =
      left.extent.zip(right.extent + right_offset)(Math.max) - offset
      
  private case class ShiftedKernel[X, K[Z] <: Kernel[Z]] (
    base: Kernel[X],
    shift: Pos
  ) extends Kernel[X]:

    def positions: Iterator[Pos] =
      base.positions.map(_ + shift)
      
    def contains(v: Pos): Boolean =
      base.contains(v - shift)
      
    def label(v: Pos): Option[X] =
      base.label(v - shift)
      
    val offset: Pos = base.offset + shift
    export base.size
    
    override def translate(v: Pos): Kernel[X] =
      ShiftedKernel(base, shift + v)
      
  private case class FilteredKernel[X] (
    base: Kernel[X],
    f: (Pos, X) => Boolean
  ) extends Kernel[X]:
    
    def positions: Iterator[Pos] =
      base.positions.filter(v => f(v, base.label(v).get))
      
    def contains(v: Pos): Boolean =
      base.contains(v) && f(v, base.label(v).get)
      
    def label(v: Pos): Option[X] =
      Option.when(contains(v))(base.label(v)).flatten
      
    export base.{offset, size}
    
  private case class UnionKernel[X, Y] (
    left: Kernel[X],
    right: Kernel[Y]
  ) extends Kernel[(Option[X], Option[Y])]:
  
    def positions: Iterator[Pos] =
      lazy val right_positions =
        val left_positions = left.positions.toSet
        right.positions.filterNot(left_positions.contains)
      left.positions ++ right_positions
      
    def contains(v: Pos): Boolean =
      left.contains(v) || right.contains(v)
      
    def label(v: Pos): Option[(Option[X], Option[Y])] =
      Option.when(contains(v))(left.label(v), right.label(v))
      
    val offset: Pos = left.offset.zip(right.offset)(Math.min)
    val size: Pos = left.extent.zip(right.extent)(Math.max) - offset
    
  private case class IntersectionKernel[X, Y] (
    left: Kernel[X],
    right: Kernel[Y]
  ) extends Kernel[(X, Y)]:
    
    def positions: Iterator[Pos] =
      left.positions.filter(right.contains)
        .zip(right.positions.filter(left.contains))
        .map((v, _) => v)
      
    def contains(v: Pos): Boolean =
      left.contains(v) && right.contains(v)
      
    def label(v: Pos): Option[(X, Y)] =
      left.label(v).zip(right.label(v))
      
    val offset: Pos = left.offset.zip(right.offset)(Math.max)
    val size: Pos = left.extent.zip(right.extent)(Math.min) - offset
  
  private case class DifferenceKernel[X, Y](
    left: Kernel[X],
    right: Kernel[Y]
  ) extends Kernel[X]:
    
    def positions: Iterator[Pos] =
      left.positions.filterNot(right.contains)
    
    def contains(v: Pos): Boolean =
      left.contains(v) && !right.contains(v)
    
    def label(v: Pos): Option[X] =
      Option.when(contains(v))(left.label(v)).flatten
    
    export left.{offset, size}
    
  private case class PaintedKernel[X, Y] (
    base: Kernel[X],
    f: (Pos, => X) => Y
  ) extends Kernel[Y]:
    
    export base.{positions, contains, offset, size}
    
    def label(v: Pos): Option[Y] =
      base.label(v).map(l => f(v - base.offset, l))
    
    override def paint[Z](g: (Pos, => Y) => Z): Kernel[Z] =
      base.paint((v, x) => g(v - base.offset, f(v - base.offset, x)))
      
  private case class SetKernel (
    private val pos: Iterable[Pos]
  ) extends Shape:
    
    def positions: Iterator[Pos] = pos.iterator
    private lazy val posSet = pos.toSet
    export posSet.contains
    
    override val dim = pos.headOption.map(_.dim).getOrElse(0)
    lazy val offset = positions.reduceOption(_.zip(_)(Math.min)).getOrElse(Pos.zero)
    lazy val size = positions.reduceOption(_.zip(_)(Math.max)).getOrElse(Pos.zero)
      + Pos.one(dim) - offset
  
  given Conversion[Pos, Kernel[Unit]] with
    def apply(v: Pos): Kernel[Unit] = SetKernel(Set(v))
    
  given Conversion[Iterable[Pos], Kernel[Unit]] with
    def apply(v: Iterable[Pos]): Kernel[Unit] = SetKernel(v.toSet)
    
  given (using kernel: Kernel[?]): Conversion[Pos, Int] with
    def apply(pos: Pos): Int = kernel.indexOf(pos)
    
  given (using kernel: Kernel[?]): Conversion[Int, Pos] with
    def apply(index: Int): Pos = kernel.posById(index)
    
  extension (pos: Pos)
    @targetName("translate")
    def + (kernel: Kernel[?]): Kernel[?] = kernel.translate(pos)