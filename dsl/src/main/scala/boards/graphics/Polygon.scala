package boards.graphics

import Polygon.*
import boards.math.algebra.Bijection.AffineBijection
import boards.math.ops.AffineOps.Affine
import boards.math.vector.Bounds.BoundsF
import boards.math.vector.{Bounds, Vec}
import boards.math.vector.Vec.VecF
import boards.math.algebra.Algebra.given
import boards.math.algebra.Bijection
import boards.math.algebra.Bijection.given
import io.circe.*
import io.circe.generic.auto.*
import io.circe.parser.*
import io.circe.syntax.*

import scala.collection.mutable

sealed trait Polygon extends Affine[Float, Polygon] derives Codec.AsObject:
  
  protected val memo: mutable.Map[Float, BoundsF] = mutable.Map.empty
  
  lazy val circumscribedBounds: BoundsF = Bounds.bounding(vertices*)
  def inscribedBounds (aspectRatio: Float = 1.0F): BoundsF
  lazy val inscribedBounds: BoundsF = inscribedBounds(1.0F)
  def vertices: Seq[VecF]
  
  def contains (v: VecF): Boolean
  
  def mapAffine (f: AffineBijection[Float, Float]): Polygon =
    TransformedPolygon(this, f)
    
  def translate (offset: VecF): Polygon = mapAffine(Bijection.Translate(offset))
  def flip (axis: Int): Polygon = mapAffine(Bijection.Flip(axis))
  def rotate (from: Int, to: Int): Polygon = mapAffine(Bijection.Rotate(from, to))
  def scale (factors: VecF): Polygon = mapAffine(Bijection.Scale(factors))
  
  def stretchTo (bounds: BoundsF): Polygon =
    this
      .translate(-circumscribedBounds.start)
      .scale(bounds.length / circumscribedBounds.length)
      .translate(bounds.start)
    
  def fitTo (bounds: BoundsF): Polygon =
    val factor = (bounds.length / circumscribedBounds.length).components.min
    this
      .translate(-circumscribedBounds.start)
      .scale(factor)
      .translate(bounds.start + (bounds.length - (circumscribedBounds.length * factor)) / 2.0F)
    
  def fitTo (shape: Polygon): Polygon =
    fitTo(shape.inscribedBounds(aspectRatio))
    
  def aspectRatio: Float = circumscribedBounds.aspectRatio
  def centre: VecF = circumscribedBounds.centre

object Polygon:
  
  case object Rectangle extends Polygon:
    
    def inscribedBounds (aspectRatio: Float): BoundsF =
      memo.getOrElseUpdate(aspectRatio,
        Bounds.fromOrigin(Vec(aspectRatio, 1.0F)).fitTo(circumscribedBounds)
      )
    
    def vertices: Seq[VecF] =
      Seq(Vec(0.0F, 0.0F), Vec(1.0F, 0.0F), Vec(1.0F, 1.0F), Vec(0.0F, 1.0F))
      
    def contains (v: VecF): Boolean =
      0.0F <= v.x && v.x <= 1.0F && 0.0F <= v.y && v.y <= 1.0F
      
  case object Hexagon extends Polygon:
    
    private val l_short_diag = 6.9F
    private val l_radius     = 0.5F * l_short_diag
    private val l_spike      = l_short_diag * 0.5F / Math.sqrt(3.0F).toFloat
    private val l_side       = 2.0F * l_spike
    private val l_offset     = 3.0F * l_spike
    private val l_long_diag  = 4.0F * l_spike
    
    def inscribedBounds (aspectRatio: Float): BoundsF =
      memo.getOrElseUpdate(aspectRatio, {
        val x = l_side / (aspectRatio + 2.0F * l_spike / l_short_diag)
        Bounds.between(Vec(-x, -x), Vec(x, x))
      })
      
    val vertices: Seq[VecF] = Seq (
      Vec(0.0F, l_side),
      Vec(-l_radius, l_spike),
      Vec(-l_radius, -l_spike),
      Vec(0.0F, -l_side),
      Vec(l_radius, -l_spike),
      Vec(l_radius, l_spike),
    )
    
    def contains (v: VecF): Boolean =
      v.x >= -l_radius &&
      v.x <= l_radius &&
      (v.x / l_radius) + (v.y / l_side) <= 1.0F &&
      (-v.x / l_radius) + (v.y / l_side) <= 1.0F &&
      (v.x / l_radius) + (-v.y / l_side) <= 1.0F &&
      (-v.x / l_radius) + (-v.y / l_side) <= 1.0F
    
  case class TransformedPolygon (
    base: Polygon,
    f: AffineBijection[Float, Float],
  ) extends Polygon:
    
    def vertices: Seq[VecF] =
      base.vertices.map(_.toUnbounded).map(f.apply).map(_.toFinite)
    
    def inscribedBounds (aspectRatio: Float): BoundsF =
      base.inscribedBounds(aspectRatio * base.aspectRatio / this.aspectRatio).mapAffine(f)
      
    def contains (v: VecF): Boolean =
      base.contains(f.inverse(v.toUnbounded).toFinite)
    
  enum Orientation derives Codec.AsObject:
    case Vertical, Horizontal
  
  enum TriangleOrientation derives Codec.AsObject:
    case Up, Down, Left, Right
    def isVertical: Boolean = this match
      case Up | Down => true
      case Left | Right => false
    def isHorizontal: Boolean = !isVertical