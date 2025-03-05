package boards.math.vector

import boards.graphics.{Colour, Shape}
import boards.math.vector.Bounds.{BoundsF, BoundsI, given}
import boards.math.vector.Embedding.*
import boards.math.vector.Vec.{VecF, VecI}
import boards.math.vector.Region.RegionI
import boards.math.algebra.Algebra.{*, given}
import boards.math.algebra.Bijection
import boards.math.algebra.Bijection.AffineBijection
import boards.util.Codecs.{*, given}
import boards.math.Conversions.{*, given}
import boards.math.ops.AffineOps.Affine
import boards.math.vector.RegionMap.{HasRegionMap, RegionMapI}
import io.circe.Codec
import io.circe.generic.auto.*

/** An embedding of a [[RegionI]] in the plane. */
sealed trait Embedding
  extends HasRegionMap[Int, Tile],
          Affine[Float, Embedding]
  derives Codec.AsObject:
  
  protected def keys: RegionI
  def region: RegionMapI[Tile] = keys.withLabels(tile)
  
  /** Convert a position from embedded-space to logical-space.
    * Useful for converting a cursor position back to game coordinates. */
  def toLogicalSpaceOpt (v: VecF): Option[VecI]
  def toLogicalSpace (v: VecF): VecI = toLogicalSpaceOpt(v).get
  
  /** Convert a position from logical-space to embedded-space.
    * Useful for determining where a board tile should be rendered. */
  def toEmbeddedSpaceOpt (v: VecI): Option[BoundsF]
  def toEmbeddedSpace (v: VecI): BoundsF = toEmbeddedSpaceOpt(v).get
  
  lazy val logicalPositions: LazyList[VecI] = region.positions
  lazy val embeddedPositions: LazyList[BoundsF] = logicalPositions.map(toEmbeddedSpace)
  
  def containsLogical (v: VecI): Boolean = region.contains(v)
  def containsEmbedded (v: VecF): Boolean = toLogicalSpaceOpt(v).isDefined
  
  def logicalBounds: BoundsI = region.bounds
  def embeddedBounds: BoundsF
  
  def logicalSize: VecI = logicalBounds.size
  def embeddedSize: VecF = embeddedBounds.size
  
  def inLogicalBounds (v: VecI): Boolean = logicalBounds.contains(v)
  def inEmbeddedBounds (v: VecF): Boolean = embeddedBounds.contains(v)
  
  def dim: Int = logicalBounds.dim
  
  def shapeOpt (v: VecI): Option[Shape]
  def shape (v: VecI): Shape = shapeOpt(v).get
  
  def colourOpt (v: VecI): Option[Colour]
  def colour (v: VecI): Colour = colourOpt(v).get
  
  def tileOpt (v: VecI): Option[Tile] = Option.when(containsLogical(v)):
    Tile(v, toEmbeddedSpace(v), shape(v), colour(v))
    
  def tile (v: VecI): Tile = tileOpt(v).get
  
  def mapAffine (f: AffineBijection[Float, Float]): Embedding =
    TransformedEmbedding(this, f)
  
  def translate (offset: VecF): Embedding =
    mapAffine(Bijection.Translate(offset))
  
  def flip (axis: Int): Embedding =
    mapAffine(Bijection.Flip(axis))
    
  def rotate (from: Int, to: Int): Embedding =
    mapAffine(Bijection.Rotate(from, to))
  
  def scale (scale: VecF): Embedding =
    mapAffine(Bijection.Scale(scale))
  
  def pad (start: VecF = Vec.zero[Float](2), end: VecF = Vec.zero[Float](2)): Embedding =
    PaddedEmbedding(this, start, end)
  def pad (x: Float): Embedding = pad(Vec.fill(dim)(x), Vec.fill(dim)(x))
  def padX (x: Float): Embedding = pad(Vec(x, 0.0F), Vec(x, 0.0F))
  def padY (x: Float): Embedding = pad(Vec(0.0F, x), Vec(0.0F, x))
  def padLeft (x: Float): Embedding = pad(start = Vec(x, 0.0F))
  def padRight (x: Float): Embedding = pad(end = Vec(x, 0.0F))
  def padBottom (x: Float): Embedding = pad(start = Vec(0.0F, x))
  def padTop (x: Float): Embedding = pad(end = Vec(0.0F, x))
  
  /** Stretch and translate this embedding so as to fill as much of the given area as possible.
    * @param container The area to fill.
    */
  def fitTo (container: BoundsF): Embedding =
    if embeddedBounds.isEmpty || embeddedBounds.isInfinite || container.isEmpty || container.isInfinite then this else
      val factor = (container.size / embeddedSize).components.min
      val result = this
        .scale(Vec.fill(dim)(factor))
        .translate(container.start - embeddedBounds.start
          + ((container.size - (embeddedBounds.size * factor)) / 2.0F))
      result
  
  def join (that: Embedding, align: Align): Embedding =
    JoinedEmbedding(this, that, align)
  
  def paint (colours: VecI => Colour): Embedding =
    PaintedEmbedding(this, logicalPositions.map(v => v -> colours(v)).toMap)
    
  def paintSolid (colour: Colour): Embedding = paint(_ => colour)

object Embedding:
  
  case class Tile (
    logicalPosition: VecI,
    embeddedPosition: BoundsF,
    shape: Shape,
    colour: Colour,
  ) derives Codec.AsObject
  
  def empty: Embedding = Region.empty[Int].embed
  
  def join (align: Align) (embeddings: Embedding*): Embedding =
    embeddings.foldLeft(Embedding.empty): (embedding, appendant) =>
      embedding.join(appendant, align)
      
  def stack (
    dim: Int,
    spacing: Float = 0.0F,
    reverse: Boolean = false,
  ) (embeddings: Embedding*): Embedding =
    join(Align.stack(dim, spacing=spacing, reverse=reverse))(embeddings*)
    
  def stackX (embeddings: Embedding*): Embedding = stack(0)(embeddings*)
  def stackY (embeddings: Embedding*): Embedding = stack(1, reverse=true)(embeddings*)
  
  case class RectilinearEmbedding (
    keys: RegionI,
    borders: VecF = Vec.zero[Float](2),
  ) extends Embedding:
    
    def toEmbeddedSpaceOpt (v: VecI) = Option.when(containsLogical(v)):
      val position = v.toVecF + (borders * (v - keys.bounds.start).toVecF)
      Bounds.between(position, position + Vec(1.0F, 1.0F))
    
    def toLogicalSpaceOpt (v: VecF) = Option.when(containsLogical(v.toVecI)):
      v.toVecI
    
    def shapeOpt (v: VecI) = Option.when(containsLogical(v)):
      Shape.Rectangle
      
    def colourOpt (v: VecI) = Option.when(containsLogical(v)):
      Colour.White
      
    val embeddedBounds: BoundsF = keys.bounds.toBoundsF
      .extendEnd(borders * (logicalSize.toVecF - Vec.fill(dim)(1.0F)))
  
  case class TransformedEmbedding (
    base: Embedding,
    f: AffineBijection[Float, Float],
  ) extends Embedding:
    
    def toEmbeddedSpaceOpt (v: VecI) =
      base.toEmbeddedSpaceOpt(v).map(_.map(f))
      
    def toLogicalSpaceOpt (v: VecF) =
      base.toLogicalSpaceOpt(f.inverse(v.toUnbounded).toFinite)
      
    def shapeOpt (v: VecI) = base.shapeOpt(v)
    def colourOpt (v: VecI) = base.colourOpt(v)
    
    val embeddedBounds = base.embeddedBounds.map(f)
    
    val keys = base.keys
    
  // For external padding.
  case class PaddedEmbedding (
    base: Embedding,
    start: VecF = Vec.zero[Float](2),
    end: VecF = Vec.zero[Float](2),
  ) extends Embedding:
    
    def toEmbeddedSpaceOpt (v: VecI) = base.toEmbeddedSpaceOpt(v)
    
    def toLogicalSpaceOpt (v: VecF) = base.toLogicalSpaceOpt(v)
    
    def shapeOpt (v: VecI) = base.shapeOpt(v)
    def colourOpt (v: VecI) = base.colourOpt(v)
    
    val embeddedBounds = base.embeddedBounds.extendStart(start).extendEnd(end)
    
    val keys = base.keys
    
    override def pad (start: VecF, end: VecF) =
      PaddedEmbedding(base, this.start + start, this.end + end)
      
  case class JoinedEmbedding (
    left: Embedding,
    right: Embedding,
    alignment: Align,
  ) extends Embedding:
  
    private val embeddedOffset = alignment.relativeOffset(left.embeddedBounds, right.embeddedBounds)
    private val logicalOffset = alignment.relativeOffset (
      left.bounds.toBoundsF,
      right.bounds.toBoundsF
    ).toVecI
    
    val keys = left.join(right, alignment)
    
    def toEmbeddedSpaceOpt (v: VecI) =
      if left.contains(v) then left.toEmbeddedSpaceOpt(v)
      else right.toEmbeddedSpaceOpt(v - logicalOffset).map(_.translate(embeddedOffset))
    
    def toLogicalSpaceOpt (v: VecF) =
      left.toLogicalSpaceOpt(v) orElse right.toLogicalSpaceOpt(v - embeddedOffset)
      
    def shapeOpt (v: VecI) =
      if left.contains(v) then left.shapeOpt(v) else right.shapeOpt(v - logicalOffset)
      
    def colourOpt (v: VecI) =
      if left.contains(v) then left.colourOpt(v) else right.colourOpt(v - logicalOffset)
    
    val embeddedBounds = left.embeddedBounds | right.embeddedBounds.translate(embeddedOffset)
    
  case class PaintedEmbedding (
    base: Embedding,
    colours: Map[VecI, Colour],
  ) extends Embedding:
    
    def toEmbeddedSpaceOpt (v: VecI) = base.toEmbeddedSpaceOpt(v)
    def toLogicalSpaceOpt (v: VecF) = base.toLogicalSpaceOpt(v)
    
    def shapeOpt (v: VecI) = base.shapeOpt(v)
    def colourOpt (v: VecI) = Option.when(containsLogical(v)):
      colours(v)
    
    val embeddedBounds = base.embeddedBounds
    
    val keys = base.keys
  
  extension (region: RegionI)
    /** Create an embedding of this [[Region]] in the plane.
      * A default embedding consists of equal-sized square tiles with no additional spacing.
      */
    def embed: RectilinearEmbedding = RectilinearEmbedding(region)