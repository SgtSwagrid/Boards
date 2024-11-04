package boards.math.kernel

import boards.imports.math.{*, given}
import Align.*
import boards.graphics.Colour

import scala.annotation.{tailrec, targetName}

trait Kernel[+X]:
  import Kernel.*
  
  def positions: Iterator[VecI]
  
  def contains(v: VecI): Boolean
  def contains(v: Int*): Boolean = contains(VecI(v*))
  
  def label(v: VecI): Option[X]
  def label(v: Int*): Option[X] = label(VecI(v*))
  def labels: Iterator[X] = positions.map(label(_).get)
  
  def offset: VecI
  def size: VecI
  def extent: VecI = offset + size
  def dim: Int = size.dim
  
  final def left: Int = offset.x
  final def right: Int = extent.x
  final def width: Int = size.x
  
  final def bottom: Int = offset.y
  final def top: Int = extent.y
  final def height: Int = size.y
  
  final def front: Int = offset.z
  final def back: Int = extent.z
  final def depth: Int = size.z
  
  final def inBounds(v: VecI): Boolean = offset <= v && v < extent
    
  def window(offset: VecI, size: VecI): Kernel[X] =
    if offset <= this.offset && offset + size >= extent then this
    else IntersectionKernel(Kernel.box(size).translate(offset), this).paintLabels(_(1))
    
  def slice(dim: Int, min: Int, max: Int): Kernel[X] =
    window(offset.update(dim, min), size.update(dim, max - min + 1))
    
  def col(j: Int): Kernel[X] = slice(0, j, j)
  def row(i: Int): Kernel[X] = slice(1, i, i)
  
  def translate(v: VecI): Kernel[X] =
    if v.isZero then this else ShiftedKernel(this, v)
  def translate(v: Int*): Kernel[X] = translate(VecI(v*))
  
  def shiftLeft(n: Int): Kernel[X] = translate(Vec.left[Int] * n)
  def shiftRight(n: Int): Kernel[X] = translate(Vec.right[Int] * n)
  def shiftUp(n: Int): Kernel[X] = translate(Vec.up[Int] * n)
  def shiftDown(n: Int): Kernel[X] = translate(Vec.down[Int] * n)
  
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
  
  def map(f: VecI => VecI): Kernel[X] = MappedKernel(this, f)
  
  def filter(f: (VecI, X) => Boolean): Kernel[X] = FilteredKernel(this, f)
  def erode(f: VecI => Boolean): Kernel[X] = filter((v, _) => f(v))
  def filterLabels(f: X => Boolean): Kernel[X] = filter((_, x) => f(x))
  
  def paint[Y](f: (VecI, => X) => Y): Kernel[Y] = PaintedKernel(this, f)
  def paint[Y](f: VecI => Y): Kernel[Y] = paint((v, _) => f(v))
  
  def paintPartial[X2 >: X, Y](f: VecI => Option[Y]): Kernel[X2 | Y] =
    paint((v, x) => f(v).getOrElse(x))
  
  def paintSolid[Y](y: Y): Kernel[Y] = paint((_, _) => y)
  
  def paintLabels[Y](f: X => Y): Kernel[Y] = paint((_, x) => f(x))
  
  def replace[Y](y: (Kernel[?], Y)*): Kernel[X | Y] =
    y.foldLeft[Kernel[X | Y]](this):
      case (k, (v, y)) =>
        val vs = v.positions.toSet
        k.paintPartial(x => Option.when(vs.contains(x))(y))
        
  def zipWithPosition: Kernel[(VecI, X)] = paint((v, x) => (v, x))
  
  def neighbours(v: VecI)(using M: EnumerableMetric[Int]): Kernel[X] =
    (this & M.neighbours(v)).paintLabels(_(0))
    
  def ball(v: VecI, rmax: Int, rmin: Int = 0)(using M: EnumerableMetric[Int]): Kernel[X] =
    (this & M.ball(v, rmax, rmin)).paintLabels(_(0))
    
  lazy val posById: IndexedSeq[VecI] = positions.toIndexedSeq
  lazy val indexOf: Map[VecI, Int] = posById.zipWithIndex.toMap
    
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
    def label(v: VecI): Option[Unit] = Option.when(contains(v))(())
  
  def apply(v: VecI*): Shape = SetKernel(v)
  def apply(v: Iterable[VecI]): Shape = SetKernel(v)
  
  def apply[X](v: (VecI, X)*): Kernel[X] =
    val colours = Map(v*)
    Kernel(v.map(_(0))).paint(colours.apply)
    
  def point(v: VecI): Shape = VecKernel(v)
  
  def box(size: VecI): Shape = BoxKernel(size)
  def box(size: Int*): Shape = box(VecI(size*))
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
  
  def empty(size: VecI): Kernel[Nothing] = EmptyKernel(size)
  def empty(size: Int*): Kernel[Nothing] = empty(VecI(size*))
  def empty: Kernel[Nothing] = EmptyKernel()
  def infinite: Shape = InfiniteKernel
  
  private case object InfiniteKernel extends Shape:
    
    def positions: Iterator[VecI] = Iterator.iterate(VecI.zero)(_ + VecI(1))
    def contains(v: VecI): Boolean = true
    val offset: VecI = VecI.zero
    val size: VecI = VecI.zero
  
  private case class EmptyKernel (
    size: VecI = VecI.zero,
  ) extends Kernel[Nothing]:
    
    def positions: Iterator[VecI] = Iterator.empty
    def contains(v: VecI): Boolean = false
    def label(v: VecI): None.type = None
    val offset: VecI = VecI.zero
    
  private class VecKernel (
    vec: VecI,
  ) extends Shape:
    
    def positions: Iterator[VecI] = Iterator(vec)
    def contains(v: VecI): Boolean = v == vec
    def offset: VecI = vec
    def size: VecI = VecI.one(vec.dim)
  
  private case class BoxKernel (
    size: VecI,
  ) extends Shape:
    
    def positions: Iterator[VecI] =
      
      @tailrec
      def rec(d: VecI, prefix: Iterator[VecI] = Iterator(VecI.zero)): Iterator[VecI] =
        d.toSeq match
          case x +: v =>
            rec(VecI(v*), for u <- prefix; i <- Iterator.range(0, x) yield u :+ i)
          case _ => prefix
      
      rec(size)
    
    def contains(v: VecI): Boolean = VecI.zero <= v && v < size
    
    val offset: VecI = VecI.zero
  
  private case class RotatedKernel[+X] (
    base: Kernel[X],
    from: Int,
    to: Int,
  ) extends Kernel[X]:
    
    def positions: Iterator[VecI] =
      base.positions.map(_.rotate(from, to))
    
    def contains(v: VecI): Boolean =
      base.contains(v.rotate(to, from))
    
    def label(v: VecI): Option[X] =
      base.label(v.rotate(to, from))
    
    val offset: VecI = base.offset
      .update(from, 1 - base.extent(to))
      .update(to, base.offset(from))
    
    val size: VecI = base.size.rotate(to, from).flip(to)
    
    override def rotate(from: Int, to: Int): Kernel[X] =
      if from == this.to && to == this.from then base
      else super.rotate(from, to)
    
  private case class FlippedKernel[+X] (
    base: Kernel[X],
    axis: Int,
  ) extends Kernel[X]:
    
    def positions: Iterator[VecI] =
      base.positions.map(_.flip(axis))
    
    def contains(v: VecI): Boolean =
      base.contains(v.flip(axis))
    
    def label(v: VecI): Option[X] =
      base.label(v.flip(axis))
      
    val offset: VecI = base.offset.update(axis, 1 - base.extent(axis))
    
    export base.size
    
    override def flip(axis: Int): Kernel[X] =
      if axis == this.axis then base
      else super.flip(axis)
      
  private case class JoinedKernel[X, Y] (
    base1: Kernel[X],
    base2: Kernel[Y],
    align: Align,
  ) extends Kernel[X | Y]:
    
    private val right_offset: VecI =
      align.relativeOffset(base1.size, base2.size) + (base1.offset - base2.offset)
    
    def positions: Iterator[VecI] =
      lazy val right_positions =
        val left_positions = base1.positions.toSet
        base2.positions.map(_ + right_offset).filterNot(left_positions.contains)
      base1.positions ++ right_positions
      
    def contains(v: VecI): Boolean =
      base1.contains(v) || base2.contains(v - right_offset)
      
    def label(v: VecI): Option[X | Y] =
      base2.label(v - right_offset).orElse(base1.label(v))
      
    val offset: VecI =
      base1.offset.zip(base2.offset + right_offset)(Math.min)
      
    val size: VecI =
      base1.extent.zip(base2.extent + right_offset)(Math.max) - offset
      
  private case class ShiftedKernel[X, K[Z] <: Kernel[Z]] (
    base: Kernel[X],
    shift: VecI,
  ) extends Kernel[X]:

    def positions: Iterator[VecI] =
      base.positions.map(_ + shift)
      
    def contains(v: VecI): Boolean =
      base.contains(v - shift)
      
    def label(v: VecI): Option[X] =
      base.label(v - shift)
      
    val offset: VecI = base.offset + shift
    export base.size
    
    override def translate(v: VecI): Kernel[X] =
      ShiftedKernel(base, shift + v)
      
  private case class FilteredKernel[X] (
    base: Kernel[X],
    f: (VecI, X) => Boolean
  ) extends Kernel[X]:
    
    def positions: Iterator[VecI] =
      base.positions.filter(v => f(v, base.label(v).get))
      
    def contains(v: VecI): Boolean =
      base.contains(v) && f(v, base.label(v).get)
      
    def label(v: VecI): Option[X] =
      Option.when(contains(v))(base.label(v)).flatten
      
    export base.{offset, size}
    
  private case class UnionKernel[X, Y] (
    base1: Kernel[X],
    base2: Kernel[Y],
  ) extends Kernel[(Option[X], Option[Y])]:
  
    def positions: Iterator[VecI] =
      lazy val right_positions =
        val left_positions = base1.positions.toSet
        base2.positions.filterNot(left_positions.contains)
      base1.positions ++ right_positions
      
    def contains(v: VecI): Boolean =
      base1.contains(v) || base2.contains(v)
      
    def label(v: VecI): Option[(Option[X], Option[Y])] =
      Option.when(contains(v))(base1.label(v), base2.label(v))
      
    val offset: VecI = base1.offset.zip(base2.offset)(Math.min)
    val size: VecI = base1.extent.zip(base2.extent)(Math.max) - offset
    
  private case class IntersectionKernel[X, Y] (
    base1: Kernel[X],
    base2: Kernel[Y],
  ) extends Kernel[(X, Y)]:
    
    def positions: Iterator[VecI] =
      base1.positions.filter(base2.contains)
        .zip(base2.positions.filter(base1.contains))
        .map((v, _) => v)
      
    def contains(v: VecI): Boolean =
      base1.contains(v) && base2.contains(v)
      
    def label(v: VecI): Option[(X, Y)] =
      base1.label(v).zip(base2.label(v))
      
    val offset: VecI = base1.offset.zip(base2.offset)(Math.max)
    val size: VecI = base1.extent.zip(base2.extent)(Math.min) - offset
  
  private case class DifferenceKernel[X, Y](
    base1: Kernel[X],
    base2: Kernel[Y],
  ) extends Kernel[X]:
    
    def positions: Iterator[VecI] =
      base1.positions.filterNot(base2.contains)
    
    def contains(v: VecI): Boolean =
      base1.contains(v) && !base2.contains(v)
    
    def label(v: VecI): Option[X] =
      Option.when(contains(v))(base1.label(v)).flatten
    
    export base1.{offset, size}
  
  private case class MappedKernel[X] (
    base: Kernel[X],
    f: VecI => VecI,
  ) extends Kernel[X]:
    
    def positions: Iterator[VecI] =
      base.positions.map(f).distinct
      
    def contains(v: VecI): Boolean =
      positions.contains(v)
      
    def label(v: VecI): Option[X] =
      base.positions.find(f(_) == v).flatMap(base.label)
    
    lazy val offset: VecI = positions.reduce(_.zip(_)(Math.min))
    lazy val size: VecI = positions.reduce(_.zip(_)(Math.max)).map(_ + 1) - offset
    
  private case class PaintedKernel[X, Y] (
    base: Kernel[X],
    f: (VecI, => X) => Y,
  ) extends Kernel[Y]:
    
    export base.{positions, contains, offset, size}
    
    def label(v: VecI): Option[Y] =
      base.label(v).map(l => f(v - base.offset, l))
    
    override def paint[Z](g: (VecI, => Y) => Z): Kernel[Z] =
      base.paint((v, x) => g(v - base.offset, f(v - base.offset, x)))
      
  private case class SetKernel (
    private val pos: Iterable[VecI],
  ) extends Shape:
    
    def positions: Iterator[VecI] = pos.iterator
    private lazy val posSet = pos.toSet
    export posSet.contains
    
    override val dim = pos.headOption.map(_.dim).getOrElse(0)
    lazy val offset = positions.reduceOption(_.zip(_)(Math.min)).getOrElse(VecI.zero)
    lazy val size = positions.reduceOption(_.zip(_)(Math.max)).getOrElse(VecI.zero)
      + VecI.one(dim) - offset
  
  type Ker = Kernel[?]
  
  given Conversion[VecI, Shape] = Kernel.point
  given Conversion[Iterable[VecI], Kernel[Unit]] = vs => SetKernel(vs.toSet)
  
  extension (v: VecI) (using k: Kernel[?])
    def toId: Int = k.indexOf(v)
    
  extension (i: Int) (using k: Kernel[?])
    def toVec: VecI = k.posById(i)