package util.math.kernel

import Align.*
import util.math.Pos.{*, given}
import util.math.Vec

sealed trait Align:
  
  def relativeOffset(l: Pos, r: Pos): Pos =
    Vec(
      (0 until Math.max(l.dim, r.dim)).map: d =>
        axisOffset(l(d), r(d), d)
    *)
  
  protected def axisOffset(l: Int, r: Int, d: Int): Int
  
  def shift(offset: Vec[Int]): Align =
    ShiftedAlign(this, offset)

object Align:
  
  def apply(axes: AxisAlign*): Align = ExplicitAlign(axes*)
  def uniform(align: AxisAlign): Align = UniformAlign(align)
  def stack(dim: Int, align: Align = Center(), spacing: Int = 0, reverse: Boolean = false): Align =
    StackedAlign(dim, align, spacing, reverse)
  
  private case class ExplicitAlign(axes: AxisAlign*) extends Align:
    protected def axisOffset(l: Int, r: Int, d: Int): Int =
      axes(d).axisOffset(l, r)
  
  private case class UniformAlign(align: AxisAlign) extends Align:
    protected def axisOffset(l: Int, r: Int, d: Int): Int =
      align.axisOffset(l, r)
      
  private case class StackedAlign (
    dim: Int,
    align: Align = Center(),
    spacing: Int = 0,
    reverse: Boolean = false
  ) extends Align:
    protected def axisOffset(l: Int, r: Int, d: Int): Int =
      if d == dim then Mate(reverse, spacing).axisOffset(l, r) else align.axisOffset(l, r, d)
  
  private case class ShiftedAlign(base: Align, offset: Vec[Int]) extends Align:
    protected def axisOffset(l: Int, r: Int, d: Int): Int =
      base.axisOffset(l, r, d) + offset(d)
  
  sealed trait AxisAlign:
    def axisOffset(l: Int, r: Int): Int
    
  case class Mate(reverse: Boolean = false, offset: Int = 0) extends AxisAlign:
    def axisOffset(l: Int, r: Int): Int =
      offset + (if !reverse then l else -r)
  
  case class Flush(reverse: Boolean = false, offset: Int = 0) extends AxisAlign:
    def axisOffset(l: Int, r: Int): Int =
      offset + (if !reverse then 0 else l - r)
    
  case class Center(offset: Int = 0) extends AxisAlign:
    def axisOffset(l: Int, r: Int): Int =
      offset + ((l - r) / 2)
      
  given Conversion[AxisAlign, Align] with
    def apply(align: AxisAlign): Align = Align.uniform(align)