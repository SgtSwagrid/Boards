package boards.math.vector

import boards.graphics.{Colour, Polygon}
import boards.graphics.Polygon.Orientation
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

import scala.collection.mutable

/** An embedding of a [[RegionI]] in the plane. */
sealed trait Embedding
  extends HasRegionMap[Int, Tile],
          Affine[Float, Embedding]
  derives Codec.AsObject:
  
  protected def keys: RegionI
  final def region: RegionMapI[Tile] = keys.withLabels(tile)
  
  private val embeddings: mutable.Map[VecI, Polygon] = mutable.Map.empty
  
  protected def embed (v: VecI): Polygon
  
  /** Convert a position in logical space to a [[Polygon]] in embedded space.
    * Yields [[None]] when the position is not contained in this region.
    * @see [[toEmbeddedSpace]], [[toLogicalSpaceOpt]], [[toLogicalSpace]]
    */
  final def toEmbeddedSpaceOpt (v: VecI): Option[Polygon] =
    Option.when(containsLogical(v))(embeddings.getOrElseUpdate(v, embed(v)))
  
  /** Convert a position in logical space to a [[Polygon]] in embedded space.
    * @throws IllegalStateException when the position is not contained in this region.
    */
  final def toEmbeddedSpace (v: VecI): Polygon =
    toEmbeddedSpaceOpt(v).get
  
  protected def unembed (v: VecF): VecI
  
  /** Convert a position in embedded space to a position in logical space.
    * Will find the address of the unique tile in which the given position is contained.
    * Yields [[None]] when the position is not contained in this region.
    * @see [[toLogicalSpace]], [[toEmbeddedSpaceOpt]], [[toEmbeddedSpace]]
    */
  final def toLogicalSpaceOpt (v: VecF): Option[VecI] =
    Some(unembed(v)).filter(containsLogical)
  
  /** Convert a position in embedded space to a position in logical space.
    * Will find the address of the unique tile in which the given position is contained.
    * @throws IllegalStateException when the position is not contained in this region.
    * @see [[toLogicalSpaceOpt]], [[toEmbeddedSpaceOpt]], [[toEmbeddedSpace]]
    */
  final def toLogicalSpace (v: VecF): VecI =
    toLogicalSpaceOpt(v).get
  
  /** The set of all positions in this region in logical space.
    * Logical space describes the logical address of each tile in the region.
    * @see [[embeddedPositions]]
    */
  final lazy val logicalPositions: LazyList[VecI] = region.positions
  
  /** The set of all positions in this region in embedded space.
    * Embedded space describes the position and size of each tile in the plane.
    * @see [[logicalPositions]]
    */
  final lazy val embeddedPositions: LazyList[Polygon] = logicalPositions.map(toEmbeddedSpace)
  
  /** An axis-aligned bounding box for this region in logical space.
    * Logical space describes the logical address of each tile in the region.
    * @see [[embeddedBounds]]
    */
  final def logicalBounds: BoundsI = region.bounds
  
  /** An axis-aligned bounding box for this region in embedded space.
    * Embedded space describes the position and size of each tile in the plane.
    * @see [[logicalBounds]]
    */
  def embeddedBounds: BoundsF
  
  final def logicalSize: VecI = logicalBounds.size
  final def embeddedSize: VecF = embeddedBounds.length
  
  /** Determine whether [[v]], a logical position, is the valid address of any tile.
    * @see containsEmbedded, inLogicalBounds, inEmbeddedBounds
    */
  final def containsLogical (v: VecI): Boolean = region.contains(v)
  
  /** Determine whether [[v]], a position in the plane, is contained in the embedding of any tile.
    * @see [[containsLogical]], [[inLogicalBounds]], [[inEmbeddedBounds]]
    */
  final def containsEmbedded (v: VecF): Boolean = toLogicalSpaceOpt(v).isDefined
  
  /** Determine whether [[v]], a logical position, lies inside the region's key bounding box.
    * Note that [[v]] being in bounds does not imply that [[v]] is a valid position.
    * @see [[inEmbeddedBounds]], [[containsLogical]], [[containsEmbedded]]
    */
  final def inLogicalBounds (v: VecI): Boolean = logicalBounds.contains(v)
  
  /** Determine whether [[v]], a position in the plane, is contained in the embedding's bounding box.
    * Note that [[v]] being in bounds does not imply that [[v]] lies inside a tile.
    * @see [[inLogicalBounds]], [[containsLogical]], [[containsEmbedded]]
    */
  final def inEmbeddedBounds (v: VecF): Boolean = embeddedBounds.contains(v)
  
  final def dim: Int = logicalBounds.dim
  
  private val colours: mutable.Map[VecI, Colour] = mutable.Map.empty
  protected def getColour (v: VecI): Colour
  final def colourOpt (v: VecI): Option[Colour] =
    Option.when(containsLogical(v))(colours.getOrElseUpdate(v, getColour(v)))
  final def colour (v: VecI): Colour = colourOpt(v).get
  
  final def tileOpt (v: VecI): Option[Tile] = Option.when(containsLogical(v)):
    Tile(v, toEmbeddedSpace(v), colour(v))
    
  final def tile (v: VecI): Tile = tileOpt(v).get
  
  final def mapAffine (f: AffineBijection[Float, Float]): Embedding =
    TransformedEmbedding(this, f)
  
  final def translate (offset: VecF): Embedding =
    mapAffine(Bijection.Translate(offset))
  
  final def flip (axis: Int): Embedding =
    mapAffine(Bijection.Flip(axis))
    
  final def rotate (from: Int, to: Int): Embedding =
    mapAffine(Bijection.Rotate(from, to))
  
  final def scale (scale: VecF): Embedding =
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
      val factor = (container.length / embeddedSize).components.min
      this
        .translate(-embeddedBounds.bottomLeft)
        .scale(factor)
        .translate(container.bottomLeft + (container.length - (embeddedBounds.length * factor)) * 0.5F)
  
  def join (that: Embedding, align: Align): Embedding =
    JoinedEmbedding(this, that, align)
  
  def paint (colours: VecI => Colour): Embedding =
    PaintedEmbedding(this, logicalPositions.map(v => v -> colours(v)).toMap)
    
  def paintSolid (colour: Colour): Embedding = paint(_ => colour)

object Embedding:
  
  case class Tile (
    logicalPosition: VecI,
    embeddedPosition: Polygon,
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
    
    def embed (v: VecI): Polygon =
      val position = v.toVecF + (borders * (v - keys.bounds.start).toVecF)
      Polygon.Rectangle.stretchTo:
        Bounds.between(position, position + Vec(1.0F, 1.0F))
    
    def unembed (v: VecF): VecI = v.toVecI
      
    def getColour (v: VecI): Colour = Colour.White
      
    val embeddedBounds: BoundsF = keys.bounds.toBoundsF
      .extendEnd(Vec.one[Float](dim))
      //.extendEnd(borders * (logicalSize.toVecF - Vec.fill(dim)(1.0F)))
    
  case class HexagonalEmbedding (
    keys: RegionI,
    orientation: Orientation = Orientation.Vertical,
    borders: Float = 0.0F,
  ) extends Embedding:
    
    private val l_short_diag = 3.7F
    private val l_spike = l_short_diag * 0.5F / Math.sqrt(3.0F).toFloat
    private val l_side = 2.0F * l_spike
    private val l_offset = 3.0F * l_spike
    private val l_long_diag = 4.0F * l_spike
    private val l_inscribed = 2.0F * l_side / (1.0F + 2.0F * l_spike / l_short_diag)
    
    def embed (v: VecI): Polygon =
      Polygon.Hexagon.stretchTo:
        orientation match
          
          case Orientation.Vertical =>
            val x = v.x * l_short_diag + v.y * l_short_diag * 0.5F
            val y = v.y * l_offset
            Bounds.between(Vec(x, y), Vec(x + l_short_diag, y + l_long_diag))
            
          case Orientation.Horizontal =>
            val x = v.x * l_offset
            val y = v.y + v.x * l_short_diag * 0.5F
            Bounds.between(Vec(x, y), Vec(x + l_long_diag, y + l_short_diag))
    
    def unembed (v: VecF): VecI = orientation match
      
      case Orientation.Vertical =>
        
        val y = v.y / l_offset
        val y_int = if y < 0.0F then y.toInt - 1 else y.toInt
        val y_frac = y - y_int
        
        val x = v.x / l_short_diag - y_int * 0.5F
        val x_int = if x < 0.0F then x.toInt - 1 else x.toInt
        val x_frac = x - x_int
        
        if (x_frac * 2.0F) + (y_frac * 3.0F) < 1.0F then Vec(x_int, y_int - 1)
        else if ((1.0F - x_frac) * 2.0F) + (y_frac * 3.0F) < 1.0F then Vec(x_int + 1, y_int - 1)
        else Vec(x_int, y_int)
        
      case Orientation.Horizontal =>
        ???
      
    def getColour (v: VecI): Colour = Colour.White
      
    lazy val embeddedBounds: BoundsF =
      embeddedPositions.map(_.circumscribedBounds).reduce(_ | _)
  
  case class TransformedEmbedding (
    base: Embedding,
    f: AffineBijection[Float, Float],
  ) extends Embedding:
    
    def embed (v: VecI): Polygon =
      base.embed(v).mapAffine(f)
      
    def unembed (v: VecF): VecI =
      base.unembed(f.inverse(v.toUnbounded).toFinite)
    
    def getColour (v: VecI): Colour = base.getColour(v)
    
    val embeddedBounds = base.embeddedBounds.map(f)
    
    val keys = base.keys
    
  // For external padding.
  case class PaddedEmbedding (
    base: Embedding,
    start: VecF = Vec.zero[Float](2),
    end: VecF = Vec.zero[Float](2),
  ) extends Embedding:
    
    def embed (v: VecI): Polygon = base.embed(v)
    
    def unembed (v: VecF): VecI = base.unembed(v)
    
    def getColour (v: VecI): Colour = base.getColour(v)
    
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
    
    def embed (v: VecI): Polygon =
      if left.contains(v) then left.embed(v)
      else right.embed(v - logicalOffset).translate(embeddedOffset)
    
    def unembed (v: VecF): VecI =
      left.toLogicalSpaceOpt(v) getOrElse right.unembed(v - embeddedOffset)
      
    def getColour (v: VecI): Colour =
      if left.contains(v) then left.getColour(v) else right.getColour(v - logicalOffset)
    
    val embeddedBounds = left.embeddedBounds | right.embeddedBounds.translate(embeddedOffset)
    
  case class PaintedEmbedding (
    base: Embedding,
    colours: Map[VecI, Colour],
  ) extends Embedding:
    
    def embed (v: VecI): Polygon = base.embed(v)
    def unembed (v: VecF): VecI = base.unembed(v)
    
    def getColour (v: VecI): Colour = colours(v)
    
    val embeddedBounds = base.embeddedBounds
    
    val keys = base.keys
  
  extension (region: RegionI)
    /** Create an embedding of this [[Region]] in the plane.
      * A default embedding consists of equal-sized square tiles with no additional spacing.
      */
    def embed: RectilinearEmbedding = RectilinearEmbedding(region)
    def embedHex: HexagonalEmbedding = HexagonalEmbedding(region)