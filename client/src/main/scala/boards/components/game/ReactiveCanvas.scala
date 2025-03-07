package boards.components.game

import boards.components.game.ReactiveCanvas.loadTexture
import boards.graphics.{Colour, Scene, Polygon, Texture}
import boards.graphics.Polygon.Orientation
import boards.math.vector.{Bounds, Metric, Vec}
import boards.math.vector.Vec.{VecF, VecI}
import boards.views.GameView.{scene, sceneBus, socket}
import com.raquo.laminar.codecs.StringAsIsCodec
import com.raquo.laminar.modifiers.RenderableText
import io.circe.Decoder.state
import jdk.jfr.internal.event.EventWriter
import org.scalajs.dom.Audio
import boards.math.algebra.Algebra.{*, given}

import java.time.temporal.TemporalQueries.offset
import scala.scalajs.js.annotation.JSExportTopLevel
import com.raquo.laminar.nodes.ChildNode
import org.scalajs.dom.{CanvasRenderingContext2D, DOMRect, HTMLImageElement, MouseEvent}

import scala.collection.mutable
import boards.util.extensions.SequenceOps.*

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import org.scalajs.dom.{document, html, window, KeyCode}
import org.scalajs.dom.html.Canvas
import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.nodes.{ReactiveElement, ReactiveHtmlElement, ReactiveSvgElement}
import com.raquo.laminar.tags.HtmlTag
import boards.math.Conversions.*
import boards.math.vector.Bounds.{BoundsF, BoundsI}

class ReactiveCanvas (id: String = "canvas"):
  
  lazy val element = document.getElementById(id).asInstanceOf[Canvas]
  lazy val context = element.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
  def rect: DOMRect = element.getBoundingClientRect()
  private def rectBounds: BoundsI =
    Bounds.fromOrigin(VecI(rect.width.toInt, rect.height.toInt))
    
  private def moveTo (pos: VecF): Unit =
    context.moveTo(pos.x.toInt, pos.y.toInt)
    
  private def lineTo (pos: VecF): Unit =
    context.lineTo(pos.x.toInt, pos.y.toInt)
    
  private def setColour (colour: Colour): Unit =
    context.fillStyle = colour.hexString
    context.strokeStyle = colour.hexString
    context.globalAlpha = colour.alpha.toFloat / 255.0F
    
  private def setAlpha (alpha: Float = 1.0F): Unit =
    context.globalAlpha = alpha
    
  private def setThickness (thickness: Float): Unit =
    context.lineWidth = thickness
    
  private def outline (thickness: Float = 1.0F) (action: => Unit): Unit =
    setThickness(thickness)
    context.beginPath()
    action
    context.stroke()
    
  private def fill (action: => Unit): Unit =
    context.beginPath()
    action
    context.fill()
    
  private def fill (vertices: VecF*): Unit =
    fill:
      moveTo(vertices.head)
      vertices.tail.foreach(lineTo)
  
  def clearRect (bounds: BoundsF): Unit =
    if bounds.nonEmpty && bounds.isFinite then
      context.clearRect(bounds.left, bounds.top, bounds.width, bounds.height)
      
  def clear (): Unit =
    context.clearRect(0, 0, rect.width, rect.height)
  
  def drawImage (bounds: BoundsF, image: Texture, alpha: Float = 1.0F) =
    val pos = bounds.bottomLeft
    val size = bounds.size
    loadTexture(s"/assets/images/games/${image.file}"): img =>
      setAlpha(alpha)
      val actualPos = pos + ((1.0F - image.size) * size.toVecF) / 2.0F
      val actualSize = image.size * size.toVecF
      context.drawImage(img, actualPos.x, actualPos.y, actualSize.x, actualSize.y)
      
  def drawGlow (pos: VecF, size: VecF, colour: Colour, radius: Int) =
    if radius > 0 then
      context.shadowColor = colour.hexString
      context.shadowBlur = radius
      context.fillRect(pos.x, pos.y, size.x, size.y)
      context.shadowBlur = 0
  
  def fillRect (bounds: BoundsF, colour: Colour): Unit =
    setColour(colour)
    context.fillRect(bounds.left, bounds.bottom, bounds.width, bounds.height)
    
  def drawRect (bounds: BoundsF, colour: Colour, thickness: Float): Unit =
    if bounds.isFinite && bounds.nonEmpty then
      setColour(colour)
      setThickness(thickness)
      context.strokeRect(bounds.left, bounds.bottom, bounds.width, bounds.height)
  
  def fillCircle (centre: VecF, radius: Float, colour: Colour): Unit =
    setColour(colour)
    fill:
      context.arc(centre.x, centre.y, radius, 0, 2 * Math.PI)
      
  def fillCircle (bounds: BoundsF, colour: Colour): Unit =
    fillCircle(bounds.centre, bounds.size.components.min / 2.0F, colour)
  
  def drawCircle (centre: VecF, radius: Float, colour: Colour, thickness: Float): Unit =
    setColour(colour)
    outline(thickness):
      context.arc(centre.x, centre.y, radius, 0, 2 * Math.PI)
      
  def drawCircle (bounds: BoundsF, colour: Colour, thickness: Float): Unit =
    drawCircle(bounds.centre, bounds.size.components.min / 2.0F, colour, thickness)
      
  def fillCross (centre: VecF, radius: Float, thickness: Float, colour: Colour): Unit =
    setColour(colour)
    outline(thickness):
      context.moveTo(centre.x - radius, centre.y - radius)
      context.lineTo(centre.x + radius, centre.y + radius)
      context.moveTo(centre.x - radius, centre.y + radius)
      context.lineTo(centre.x + radius, centre.y - radius)
      
  def fillCross (bounds: BoundsF, thickness: Float, colour: Colour): Unit =
    fillCross(bounds.centre, bounds.size.components.min / 2.0F, thickness, colour)
    
  def fillPolygon (shape: Polygon, colour: Colour) =
    setColour(colour)
    fill(shape.vertices*)
  
  val updates: EventBus[Any] = new EventBus[Any]
  
  val size: Signal[VecF] =
    updates.stream.delay(10).mapTo:
      element.width = rect.width.toInt
      element.height = rect.height.toInt
      element.oncontextmenu = e =>
        e.preventDefault()
        e.stopPropagation()
      Vec(rect.width.toFloat, rect.height.toFloat)
    .startWith(Vec.one[Float](2))
    .distinct
    
  val bounds: Signal[BoundsF] =
    size.map(Bounds.fromOrigin)
  
  val apply: HtmlElement =
    canvasTag (
      onLoad --> updates,
      windowEvents(_.onResize) --> updates,
      idAttr(id),
      width("100%"),
      height("100%"),
    )

object ReactiveCanvas:
  
  private val textures: mutable.Map[String, HTMLImageElement] = mutable.Map.empty
  
  private def loadTexture (src: String) (onload: HTMLImageElement => Unit): Unit =
    textures.get(src) match
      case Some(texture) => onload(texture)
      case None =>
        val img = document.createElement("img").asInstanceOf[HTMLImageElement]
        img.src = src
        img.onload = _ =>
          textures += (src -> img)
          onload(img)