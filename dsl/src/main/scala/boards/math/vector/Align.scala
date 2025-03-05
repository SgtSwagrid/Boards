package boards.math.vector

import Align.*
import boards.math.algebra.Algebra.{*, given}
import boards.math.vector.Bounds.BoundsF
import boards.math.vector.Vec.VecF
import boards.util.Codecs.{*, given}
import io.circe.Codec
import io.circe.Encoder.encodeSeq
import io.circe.Decoder.decodeSeq

/** An alignment strategy for joining [[Bounds]]es together.
  * Determines how each [[Bounds]] should be shifted so that it aligns with another.
  */
sealed trait Align derives Codec.AsObject:
  
  /** Given this alignment, calculate the offset that must be applied to align two [[Bounds]]s.
    *
    * @param l The first [[Bounds]].
    * @param r The second [[Bounds]].
    * @return The offset that must be applied to the right [[Bounds]] to align it with the left.
    */
  final def relativeOffset (l: BoundsF, r: BoundsF): VecF =
    (l.ustart - r.ustart).toFinite + Vec:
      (0 until Math.max(l.dim, r.dim)).map: d =>
        axisOffset(l.usize.toFinite[Float](d), r.usize.toFinite[Float](d), d)
  
  /** Calculate the offset that must be applied in a single axis.
    * Has no information about the relative position of each [[Bounds]],
    * and thus assumes that both [[Bounds]]es are aligned from the origin in the positive orthant.
    * A correction is applied later to remove this assumption.
    *
    * @param l The size of the first [[Bounds]] in this axis.
    * @param r The size of the second [[Bounds]] in this axis.
    * @param d The axis along which to calculate the offset.
    * @return The offset in this axis which must be applied to the right [[Bounds]].
    */
  protected def axisOffset (l: Float, r: Float, d: Int): Float
  
  /** Apply an additional offset to this alignment.
    * After calculating the final positions of two [[Bounds]]es,
    * this offset is applied to the latter.
    *
    * @param offset The offset to apply.
    * @return A new [[Align]] with the given offset included.
    */
  def shift (offset: VecF): Align =
    ShiftedAlign(this, offset)

object Align:
  
  /** Create an [[Align]] with a distinct strategy for each axis.
    * @param axes The [[AxisAlign]] used for each corresponding axis.
    */
  def apply (axes: AxisAlign*): Align = ExplicitAlign(axes)
  
  /** Create an [[Align]] which stacks [[Bounds]]es along some specified axis.
    *
    * @param axis    The axis along which the [[Bounds]]es are stacked.
    * @param align   The alignment strategy to apply to the other axes (typically [[Flush]] or [[Centre]]).
    * @param spacing The gap between each [[Bounds]] in the stack.
    * @param reverse Whether to reverse the order of the stack.
    */
  def stack (
    dim: Int,
    align: Align = Align.centre(),
    spacing: Float = 0.0F,
    reverse: Boolean = false,
  ): Align =
    StackedAlign(dim, align, spacing, reverse)
  
  /** Aligns [[Bounds]]es so that one begins where another ends.
    *
    * @param reverse Whether to reverse the order of the [[Bounds]]es.
    * @param offset  The gap between the two [[Bounds]]es.
    */
  def mate (
    reverse: Boolean = false,
    offset: Float = 0.0F,
  ): AxisAlign = Mate(reverse, offset)
  
  /** Aligns [[Bounds]]es so that both begin or end in the same plane.
    *
    * @param reverse Whether to align on the starting or ending edge.
    * @param offset  The offset between the starting or ending edges.
    */
  def flush (
    reverse: Boolean = false,
    offset: Float = 0.0F,
  ): AxisAlign = Flush(reverse, offset)
  
  /** Aligns [[Bounds]]es by their centres.
    * Assumes corresponding dimensions are both even or both odd,
    * lest you end up with a slightly crooked alignment.
    *
    * @param offset The gap between the centres.
    */
  def centre (
    offset: Float = 0.0F,
  ): AxisAlign = Centre(offset)
  
  /** An [[Align]] with a distinct strategy for each axis.
    * @param axes The [[AxisAlign]] used for each corresponding axis.
    */
  private case class ExplicitAlign (axes: Seq[AxisAlign]) extends Align:
    def axisOffset (l: Float, r: Float, d: Int): Float =
      axes(d).axisOffset(l, r)
      
  /** An [[Align]] which stacks [[Bounds]]es along some specified axis.
    *
    * @param axis    The axis along which the [[Bounds]]es are stacked.
    * @param align   The alignment strategy to apply to the other axes (typically [[Flush]] or [[Centre]]).
    * @param spacing The gap between each [[Bounds]] in the stack.
    * @param reverse Whether to reverse the order of the stack.
    */
  private case class StackedAlign  (
    axis: Int,
    align: Align,
    spacing: Float,
    reverse: Boolean,
  ) extends Align:
    
    def axisOffset (l: Float, r: Float, d: Int): Float =
      if d == axis
      then Mate(reverse, spacing).axisOffset(l, r)
      else align.axisOffset(l, r, d)
  
  /** An [[Align]] which applies an additional offset to another [[Align]].
    * @param base The primary alignment strategy.
    * @param offset The offset to apply afterwards.
    */
  private case class ShiftedAlign  (
    base: Align,
    offset: VecF,
  ) extends Align:
    
    def axisOffset (left: Float, right: Float, dim: Int): Float =
      base.axisOffset(left, right, dim) + offset(dim)
      
    // Optimisation: Not required for correctness!
    override def shift (offset: VecF): Align =
      ShiftedAlign(base, this.offset + offset)
  
  /** An alignment strategy for a single axis.
    * Typically composed with [[Align.apply]] or [[Align.stack]] to form a full [[Align]].
    * However, when used as an [[Align]] in itself, it performs an identical alignment on all axes.
    */
  sealed trait AxisAlign extends Align derives Codec.AsObject:
    def axisOffset (l: Float, r: Float, d: Int): Float = axisOffset(l, r)
    def axisOffset (l: Float, r: Float): Float
  
  /** Aligns [[Bounds]]es so that one begins where another ends.
 *
    * @param reverse Whether to reverse the order of the [[Bounds]]es.
    * @param offset  The gap between the two [[Bounds]]es.
    */
  private case class Mate (
    reverse: Boolean,
    offset: Float,
  ) extends AxisAlign:
    
    def axisOffset (l: Float, r: Float): Float =
      offset + (if !reverse then l else -r)
  
  /** Aligns [[Bounds]]es so that both begin or end in the same plane.
    *
    * @param reverse Whether to align on the starting or ending edge.
    * @param offset The offset between the starting or ending edges.
    */
  private case class Flush (
    reverse: Boolean,
    offset: Float,
  ) extends AxisAlign:
    
    def axisOffset (l: Float, r: Float): Float =
      offset + (if !reverse then 0.0F else l - r)
  
  /** Aligns [[Bounds]]es by their centres.
    * Assumes corresponding dimensions are both even or both odd,
    * lest you end up with a slightly crooked alignment.
    * @param offset The gap between the centres.
    */
  private case class Centre (
    offset: Float,
  ) extends AxisAlign:
    
    def axisOffset (l: Float, r: Float): Float =
      offset + ((l - r) / 2)