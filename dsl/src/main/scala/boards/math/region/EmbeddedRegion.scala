package boards.math.region

import boards.graphics.{Colour, Shape}
import boards.math.region.BoundingBox.BoundingBoxS
import boards.math.region.EmbeddedRegion.*
import boards.math.region.Vec.{VecF, VecI, VecS}
import boards.math.Surd
import boards.math.region.Region.RegionI
import boards.math.Conversions.{*, given}
import boards.util.Codecs.{*, given}
import io.circe.Codec

/** An embedding of a [[RegionI]] in the plane. */
sealed trait EmbeddedRegion extends RegionMap[Int, Tile] derives Codec.AsObject:
  
  protected def getLabel (v: VecI) = Tile (
    v,
    toEmbeddedSpace(v).get,
    shape(v).get,
    colour(v).get,
  )
  
  /** Convert a position from logical-space to embedded-space.
    * Useful for determining where a board tile should be rendered. */
  def toEmbeddedSpace (v: VecI): Option[BoundingBoxS]
  
  /** Convert a position from embedded-space to logical-space.
    * Useful for converting a cursor position back to game coordinates. */
  def toLogicalSpace (v: VecF): Option[VecI]
  
  def shape (v: VecI): Option[Shape]
  def colour (v: VecI): Option[Colour]
  
  /** The range of this embedding,
    * i.e. the subset of the plane into which all positions in this region are mapped.
    * May be intentionally non-tight, for instance for embeddings with external padding.
    *
    * Differs from [[boundingBox]] in that this describes the region in embedded-space rather than logical-space.
    */
  def range: BoundingBoxS
  
  def stretch (scale: VecS): EmbeddedRegion = ScaledEmbedding(this, scale)
  def stretch (x: Surd): EmbeddedRegion = stretch(Vec(x, x))
  def hstretch (x: Surd): EmbeddedRegion = stretch(Vec(x, Surd.one))
  def vstretch (x: Surd): EmbeddedRegion = stretch(Vec(Surd.one, x))
  
  def pad (start: VecS = Vec.zero[Surd](2), end: VecS = Vec.zero[Surd](2)): EmbeddedRegion =
    PaddedEmbedding(this, start, end)
  def pad (x: Surd): EmbeddedRegion = pad(Vec(x, x), Vec(x, x))
  def hpad (x: Surd): EmbeddedRegion = pad(Vec(x, Surd.zero), Vec(x, Surd.zero))
  def vpad (x: Surd): EmbeddedRegion = pad(Vec(Surd.zero, x), Vec(Surd.zero, x))
  def lpad (x: Surd): EmbeddedRegion = pad(start = Vec(x, Surd.zero))
  def rpad (x: Surd): EmbeddedRegion = pad(end = Vec(x, Surd.zero))
  def dpad (x: Surd): EmbeddedRegion = pad(start = Vec(Surd.zero, x))
  def upad (x: Surd): EmbeddedRegion = pad(end = Vec(Surd.zero, x))
  
  /*def borders (x: VecS): EmbeddedRegion = BorderEmbedding(this, x)
  def borders (x: Surd): EmbeddedRegion = borders(Vec(x, x))
  def borders (dim: Int, x: Surd): EmbeddedRegion = borders(Vec.axis(dim) * x)
  def hborders (x: Surd): EmbeddedRegion = borders(0, x)
  def vborders (x: Surd): EmbeddedRegion = borders(1, x)*/
  
  def join (that: EmbeddedRegion, align: Align): EmbeddedRegion =
    JoinedEmbedding(this, that, align)
  
  def paint (colours: VecI => Colour): EmbeddedRegion =
    PaintedEmbedding(this, positions.map(v => v -> colours(v)).toMap)
    
  def fill (colour: Colour): EmbeddedRegion = paint(_ => colour)
  
  /** Explicitly enumerate and save all [[Tile]]s in this [[EmbeddedRegion]].
    * Will not affect the logical behaviour of this embedding,
    * but may affect performance (positively or negatively).
    * Useful for serialisation.
    */
  def explicit: ExplicitEmbedding = ExplicitEmbedding(labels, range)

object EmbeddedRegion:
  
  case class Tile (
    logicalPosition: VecI,
    embeddedPosition: BoundingBoxS,
    shape: Shape,
    colour: Colour,
  ) derives Codec.AsObject
  
  def empty: EmbeddedRegion = Region.empty[Int].embed
  
  def join (align: Align) (embeddings: EmbeddedRegion*): EmbeddedRegion =
    embeddings.foldLeft(EmbeddedRegion.empty): (embedding, appendant) =>
      embedding.join(appendant, align)
      
  def stack (
    dim: Int,
    spacing: Surd = Surd.zero,
    reverse: Boolean = false,
  ) (embeddings: EmbeddedRegion*): EmbeddedRegion =
    join(Align.stack(dim, spacing=spacing, reverse=reverse))(embeddings*)
    
  def hstack (embeddings: EmbeddedRegion*): EmbeddedRegion = stack(0)(embeddings*)
  def vstack (embeddings: EmbeddedRegion*): EmbeddedRegion = stack(1, reverse=true)(embeddings*)
  
  def explicit (tiles: Seq[Tile], range: BoundingBoxS): EmbeddedRegion =
    ExplicitEmbedding(tiles, range)
  
  case class ExplicitEmbedding (
    tiles: Seq[Tile],
    range: BoundingBoxS,
  ) extends EmbeddedRegion:
    
    val keys = Region.from(tiles.map(_.logicalPosition))
    private val tilesByPos: Map[VecI, Tile] = tiles.map(t => t.logicalPosition -> t).toMap
    
    def toEmbeddedSpace (v: VecI) =
      tilesByPos.get(v).map(_.embeddedPosition)
    
    def toLogicalSpace (v: VecF) =
      // Brute force search: without structure, there's nothing smarter we can do.
    {
      
      labels.find(_.embeddedPosition.toBoundingBoxF.inBounds(v)).map(_.logicalPosition)
    }
      
    def shape (v: VecI) = tilesByPos.get(v).map(_.shape)
    def colour (v: VecI) = tilesByPos.get(v).map(_.colour)
  
  case class RectilinearEmbedding (
    keys: RegionI,
    borders: VecS = Vec.zero[Surd](2),
  ) extends EmbeddedRegion:
    
    private lazy val tileSize = (range.size + borders).toVecF / keys.size.toBounded.toVecF
    
    def toEmbeddedSpace (v: VecI) = Option.when(contains(v)):
      val position = v.toVecS + (borders * (v - keys.boundingBox.toBounded.start).toVecS)
      BoundingBox(position, position + Vec(Surd.one, Surd.one))
    
    def toLogicalSpace (v: VecF) =
      (v - range.start.toVecF) / tileSize
      ???
    
    def shape (v: VecI) = Option.when(contains(v)):
      Shape.Rectangle
      
    def colour (v: VecI) = Option.when(contains(v)):
      Colour.White
      
    val range = keys.boundingBox.toBounded.toBoundingBoxS
      .extendEnd(borders * (size.toBounded.toVecS - Vec.fill(dim)(Surd.one)))
    
  case class ScaledEmbedding (
    keys: EmbeddedRegion,
    scale: VecS = Vec.one[Surd](2),
  ) extends EmbeddedRegion:
    
    def toEmbeddedSpace (v: VecI) =
      keys.toEmbeddedSpace(v).map(_.map(_ * scale))
    
    def toLogicalSpace (v: VecF) =
      keys.toLogicalSpace(v / scale.toVecF)
      
    def shape (v: VecI) = keys.shape(v)
    def colour (v: VecI) = keys.colour(v)
    
    val range = keys.range.map(_ * scale)
      
    override def stretch (scale: VecS) =
      ScaledEmbedding(keys, scale * this.scale)
    
  // For external padding.
  case class PaddedEmbedding (
    keys: EmbeddedRegion,
    start: VecS = Vec.zero[Surd](2),
    end: VecS = Vec.zero[Surd](2),
  ) extends EmbeddedRegion:
    
    def toEmbeddedSpace (v: VecI) = keys.toEmbeddedSpace(v)//.map(_.translate(start))
    
    def toLogicalSpace (v: VecF) = keys.toLogicalSpace(v)
    
    def shape (v: VecI) = keys.shape(v)
    def colour (v: VecI) = keys.colour(v)
    
    val range = keys.range.extendStart(start).extendEnd(end)
    
    override def pad (start: VecS, end: VecS) =
      PaddedEmbedding(keys, this.start + start, this.end + end)
      
  // For internal borders.
  /*case class BorderEmbedding (
    keys: EmbeddedRegion,
    borders: VecS = Vec.zero[Surd](2),
  ) extends EmbeddedRegion:
    
    def toEmbeddedSpace (v: VecI) = keys.toEmbeddedSpace(v)
      .map(_.translate((v - finiteBoundingBox[Int].start).toSurdVec * borders))
    
    def toLogicalSpace (v: VecS) =
      ???
    
    def shape (v: VecI) = keys.shape(v)
    def colour (v: VecI) = keys.colour(v)
    
    val range = keys.range.extendEnd(borders * (size.toBounded.toVecS - Vec.fill(dim)(Surd.one)))*/
      
  case class JoinedEmbedding (
    left: EmbeddedRegion,
    right: EmbeddedRegion,
    alignment: Align,
  ) extends EmbeddedRegion:
  
    private val embeddedOffset = alignment.relativeOffset(left.range, right.range)
    private val logicalOffset = alignment.relativeOffset (
      left.boundingBox.toBounded.toBoundingBoxS,
      right.boundingBox.toBounded.toBoundingBoxS
    ).toVecI
    
    val keys = left.join(right, alignment)
    
    def toEmbeddedSpace (v: VecI) =
      if left.contains(v) then left.toEmbeddedSpace(v)
      else right.toEmbeddedSpace(v - logicalOffset).map(_.translate(embeddedOffset))
    
    def toLogicalSpace (v: VecF) =
      left.toLogicalSpace(v) orElse right.toLogicalSpace(v - embeddedOffset.toVecF)
      
    def shape (v: VecI) =
      if left.contains(v) then left.shape(v) else right.shape(v - logicalOffset)
      
    def colour (v: VecI) =
      if left.contains(v) then left.colour(v) else right.colour(v - logicalOffset)
    
    val range = left.range | right.range.translate(embeddedOffset)
    
  case class PaintedEmbedding (
    keys: EmbeddedRegion,
    colours: Map[VecI, Colour],
  ) extends EmbeddedRegion:
    
    def toEmbeddedSpace (v: VecI) = keys.toEmbeddedSpace(v)
    def toLogicalSpace (v: VecF) = keys.toLogicalSpace(v)
    
    def shape (v: VecI) = keys.shape(v)
    def colour (v: VecI) = Option.when(contains(v)):
      colours(v)
    
    val range = keys.range
  
  extension (region: RegionI)
    /** Create an embedding of this [[Region]] in the plane.
      * A default embedding consists of equal-sized square tiles with no additional spacing.
      */
    def embed: RectilinearEmbedding = RectilinearEmbedding(region)