package boards.math.region

import Align.*
import boards.math.Surd
import boards.math.Algebra.*
import boards.util.Codecs.{*, given}
import io.circe.Codec
import io.circe.Encoder.encodeSeq
import io.circe.Decoder.decodeSeq

/** An alignment strategy for joining [[BoundingBox]]es together.
  * Determines how each [[BoundingBox]] should be shifted so that it aligns with another.
  */
sealed trait Align derives Codec.AsObject:
  
  /** Given this alignment, calculate the offset that must be applied to align two [[BoundingBox]]s.
    * @param l The first [[BoundingBox]].
    * @param r The second [[BoundingBox]].
    * @return The offset that must be applied to the right [[BoundingBox]] to align it with the left.
    */
  final def relativeOffset (l: BoundingBox[Surd], r: BoundingBox[Surd]): Vec[Surd] =
    (l.start - r.start) + Vec:
      (0 until Math.max(l.dim, r.dim)).map: d =>
        axisOffset(l.size(d), r.size(d), d)
  
  /** Calculate the offset that must be applied in a single axis.
    * Has no information about the relative position of each [[BoundingBox]],
    * and thus assumes that both [[BoundingBox]]es are aligned from the origin in the positive orthant.
    * A correction is applied later to remove this assumption.
    * @param l The size of the first [[BoundingBox]] in this axis.
    * @param r The size of the second [[BoundingBox]] in this axis.
    * @param d The axis along which to calculate the offset.
    * @return The offset in this axis which must be applied to the right [[BoundingBox]].
    */
  protected def axisOffset (l: Surd, r: Surd, d: Int): Surd
  
  /** Apply an additional offset to this alignment.
    * After calculating the final positions of two [[BoundingBox]]es,
    * this offset is applied to the latter.
    * @param offset The offset to apply.
    * @return A new [[Align]] with the given offset included.
    */
  def shift (offset: Vec[Surd]): Align =
    ShiftedAlign(this, offset)

object Align:
  
  /** Create an [[Align]] with a distinct strategy for each axis.
    * @param axes The [[AxisAlign]] used for each corresponding axis.
    */
  def apply (axes: AxisAlign*): Align = ExplicitAlign(axes)
  
  /** Create an [[Align]] which stacks [[BoundingBox]]es along some specified axis.
    * @param axis    The axis along which the [[BoundingBox]]es are stacked.
    * @param align   The alignment strategy to apply to the other axes (typically [[Flush]] or [[Centre]]).
    * @param spacing The gap between each [[BoundingBox]] in the stack.
    * @param reverse Whether to reverse the order of the stack.
    */
  def stack (
    dim: Int,
    align: Align = Align.centre(),
    spacing: Surd = Surd.zero,
    reverse: Boolean = false,
  ): Align =
    StackedAlign(dim, align, spacing, reverse)
  
  /** Aligns [[BoundingBox]]es so that one begins where another ends.
    *
    * @param reverse Whether to reverse the order of the [[BoundingBox]]es.
    * @param offset  The gap between the two [[BoundingBox]]es.
    */
  def mate (
    reverse: Boolean = false,
    offset: Surd = Surd.zero,
  ): AxisAlign = Mate(reverse, offset)
  
  /** Aligns [[BoundingBox]]es so that both begin or end in the same plane.
    *
    * @param reverse Whether to align on the starting or ending edge.
    * @param offset  The offset between the starting or ending edges.
    */
  def flush (
    reverse: Boolean = false,
    offset: Surd = Surd.zero,
  ): AxisAlign = Flush(reverse, offset)
  
  /** Aligns [[BoundingBox]]es by their centres.
    * Assumes corresponding dimensions are both even or both odd,
    * lest you end up with a slightly crooked alignment.
    *
    * @param offset The gap between the centres.
    */
  def centre (
    offset: Surd = Surd.zero,
  ): AxisAlign = Centre(offset)
  
  /** An [[Align]] with a distinct strategy for each axis.
    * @param axes The [[AxisAlign]] used for each corresponding axis.
    */
  private case class ExplicitAlign (axes: Seq[AxisAlign]) extends Align:
    def axisOffset (l: Surd, r: Surd, d: Int): Surd =
      axes(d).axisOffset(l, r)
      
  /** An [[Align]] which stacks [[BoundingBox]]es along some specified axis.
    * @param axis    The axis along which the [[BoundingBox]]es are stacked.
    * @param align   The alignment strategy to apply to the other axes (typically [[Flush]] or [[Centre]]).
    * @param spacing The gap between each [[BoundingBox]] in the stack.
    * @param reverse Whether to reverse the order of the stack.
    */
  private case class StackedAlign  (
    axis: Int,
    align: Align,
    spacing: Surd,
    reverse: Boolean,
  ) extends Align:
    
    def axisOffset (l: Surd, r: Surd, d: Int): Surd =
      if d == axis
      then Mate(reverse, spacing).axisOffset(l, r)
      else align.axisOffset(l, r, d)
  
  /** An [[Align]] which applies an additional offset to another [[Align]].
    * @param base The primary alignment strategy.
    * @param offset The offset to apply afterwards.
    */
  private case class ShiftedAlign  (
    base: Align,
    offset: Vec[Surd],
  ) extends Align:
    
    def axisOffset (left: Surd, right: Surd, dim: Int): Surd =
      base.axisOffset(left, right, dim) + offset(dim)
      
    // Optimisation: Not required for correctness!
    override def shift (offset: Vec[Surd]): Align =
      ShiftedAlign(base, this.offset + offset)
  
  /** An alignment strategy for a single axis.
    * Typically composed with [[Align.apply]] or [[Align.stack]] to form a full [[Align]].
    * However, when used as an [[Align]] in itself, it performs an identical alignment on all axes.
    */
  sealed trait AxisAlign extends Align derives Codec.AsObject:
    def axisOffset (l: Surd, r: Surd, d: Int): Surd = axisOffset(l, r)
    def axisOffset (l: Surd, r: Surd): Surd
  
  /** Aligns [[BoundingBox]]es so that one begins where another ends.
    * @param reverse Whether to reverse the order of the [[BoundingBox]]es.
    * @param offset The gap between the two [[BoundingBox]]es.
    */
  private case class Mate (
    reverse: Boolean,
    offset: Surd,
  ) extends AxisAlign:
    
    def axisOffset (l: Surd, r: Surd): Surd =
      offset + (if !reverse then l else -r)
  
  /** Aligns [[BoundingBox]]es so that both begin or end in the same plane.
    * @param reverse Whether to align on the starting or ending edge.
    * @param offset The offset between the starting or ending edges.
    */
  private case class Flush (
    reverse: Boolean,
    offset: Surd,
  ) extends AxisAlign:
    
    def axisOffset (l: Surd, r: Surd): Surd =
      offset + (if !reverse then Surd.zero else l - r)
  
  /** Aligns [[BoundingBox]]es by their centres.
    * Assumes corresponding dimensions are both even or both odd,
    * lest you end up with a slightly crooked alignment.
    * @param offset The gap between the centres.
    */
  private case class Centre (
    offset: Surd,
  ) extends AxisAlign:
    
    def axisOffset (l: Surd, r: Surd): Surd =
      offset + ((l - r) / 2)