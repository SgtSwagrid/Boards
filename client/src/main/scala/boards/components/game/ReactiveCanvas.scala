package boards.components.game

import boards.components.game.ReactiveCanvas.loadTexture
import boards.graphics.{Colour, Scene, Texture}
import boards.math.vector.{Bounds, Metric}
import boards.math.vector.Vec.VecI
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
import boards.math.vector.Bounds.BoundsI

class ReactiveCanvas (id: String = "canvas"):
  
  lazy val element = document.getElementById(id).asInstanceOf[Canvas]
  lazy val context = element.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
  def rect: DOMRect = element.getBoundingClientRect()
  
  def fillRect (pos: VecI, size: VecI, colour: Colour, alpha: Float = 1.0F): Unit =
    context.fillStyle = colour.hexString
    context.globalAlpha = alpha
    context.fillRect(pos.x, rect.height.toInt - size.y - pos.y, size.x, size.y)
    
  def fillBounds (bounds: BoundsI, colour: Colour, alpha: Float = 1.0F): Unit =
    fillRect(bounds.bottomLeft, bounds.size, colour, alpha)
  
  def fillCircle (centre: VecI, radius: Float, colour: Colour, alpha: Float = 1.0) =
    context.fillStyle = colour.hexString
    context.globalAlpha = alpha
    context.beginPath()
    context.arc(centre.x, rect.height.toInt - centre.y, radius, 0, 2 * Math.PI)
    context.fill()
  
  def drawCircle (centre: VecI, radius: Float, colour: Colour, thickness: Float, alpha: Float = 1.0) =
    context.strokeStyle = colour.hexString
    context.lineWidth = thickness
    context.globalAlpha = alpha
    context.beginPath()
    context.arc(centre.x, rect.height.toInt - centre.y, radius, 0, 2 * Math.PI)
    context.stroke()
  
  def drawCross (centre: VecI, radius: Float, colour: Colour, thickness: Float, alpha: Float = 1.0F) =
    context.strokeStyle = colour.hexString
    context.lineWidth = thickness
    context.globalAlpha = alpha
    context.beginPath()
    context.moveTo(centre.x - radius, rect.height.toInt - centre.y - radius)
    context.lineTo(centre.x + radius, rect.height.toInt - centre.y + radius)
    context.moveTo(centre.x - radius, rect.height.toInt - centre.y + radius)
    context.lineTo(centre.x + radius, rect.height.toInt - centre.y - radius)
    context.stroke()
  
  def clearRect(bounds: BoundsI) =
    if !bounds.isEmpty then
      context.clearRect(bounds.left, bounds.top, bounds.width, bounds.height)
  
  def drawImage (bounds: BoundsI, image: Texture, alpha: Float = 1.0F) =
    val pos = bounds.bottomLeft
    val size = bounds.size
    loadTexture(s"/assets/images/games/${image.file}"): img =>
      context.globalAlpha = alpha
      val actualPos = pos + ((1.0F - image.size) * size.toVecF).toVecI / 2
      val actualSize = image.size * size.toVecF
      context.drawImage(img, actualPos.x, rect.height.toInt - actualSize.y - actualPos.y, actualSize.x, actualSize.y)
      
  def drawGlow (pos: VecI, size: VecI, colour: Colour, radius: Int) =
    if radius > 0 then
      context.shadowColor = colour.hexString
      context.shadowBlur = radius
      context.fillRect(pos.x, rect.height.toInt - size.y - pos.y, size.x, size.y)
      context.shadowBlur = 0
  
  val updates: EventBus[Any] = new EventBus[Any]
  
  val size: Signal[VecI] =
    updates.stream.delay(10).mapTo:
      element.width = rect.width.toInt
      element.height = rect.height.toInt
      element.oncontextmenu = e =>
        e.preventDefault()
        e.stopPropagation()
      VecI(rect.width.toInt, rect.height.toInt)
    .startWith(VecI.zero(2))
    .distinct
    
  val bounds: Signal[BoundsI] =
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