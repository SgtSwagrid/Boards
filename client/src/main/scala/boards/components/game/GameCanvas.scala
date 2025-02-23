package boards.components.game

import boards.graphics.Scene
import boards.graphics.Scene.{Choice, PieceData}
import boards.math.region.Metric
import boards.math.region.Vec.VecI
import boards.protocol.GameProtocol.GameRequest
import boards.protocol.Room.Player
import boards.views.GameView.{scene, sceneBus, socket}
import com.raquo.laminar.codecs.StringAsIsCodec
import com.raquo.laminar.modifiers.RenderableText
import io.circe.Decoder.state
import jdk.jfr.internal.event.EventWriter
import org.scalajs.dom.{Audio, document as JSExport}
import boards.math.Conversions.{*, given}

import java.awt.Canvas
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
import io.laminext.syntax.core.*
import io.laminext.fetch.circe.{jsonRequestBody, Fetch}
import io.laminext.websocket.circe.{WebSocket, WebSocketError, WebSocketEvent}
import io.laminext.fetch.circe.fetchEventStreamBuilderSyntaxCirce
import io.laminext.websocket.circe.webSocketReceiveBuilderSyntax

class GameCanvas (scene: Signal[Scene]) extends ReactiveCanvas("game"):
  
  /** The size in pixels of a single tile from the game board. */
  val scale: Signal[Int] =
    Signal.combine(scene, size).map: (scene, size) =>
      try (size * scene.board.size.toBounded).components.min
        / scene.board.boundingBox.area.toBounded
      catch case _ => 0
    .distinct
  
  /** The offset in pixels from the corner of the canvas to the corner of the game board. */
  val offset: Signal[VecI] =
    Signal.combine(scene, size, scale).map: (scene, size, scale) =>
      (size - (scene.board.size.toBounded * scale)) / 2
    .distinct
  
  val config: Signal[GameCanvas.Config] =
    Signal.combine(size, scale, offset).map(GameCanvas.Config.apply).distinct
    
  def inBounds (pos: VecI) (using cfg: GameCanvas.Config): Boolean =
    pos >= cfg.offset && pos <= (cfg.size - cfg.offset)
  
  def canvasToGameCoords (pos: VecI) (using cfg: GameCanvas.Config): VecI =
    try (pos.flipY + cfg.size.projY - cfg.offset) / cfg.scale
    catch case _ => VecI.zero(2)
  
  /** Get the position of the top-left corner of a tile in canvas (pixel) coordinates. */
  def gameToCanvasCornerCoords (pos: VecI) (using cfg: GameCanvas.Config): VecI =
    (pos * cfg.scale + cfg.offset).flipY + cfg.size.projY - VecI(0, cfg.scale)
  
  /** Get the position of the center of a tile in canvas (pixel) coordinates. */
  def gameToCanvasCenterCoords (pos: VecI) (using cfg: GameCanvas.Config): VecI =
    gameToCanvasCornerCoords(pos) + VecI.fill(2)(cfg.scale / 2)
  
  def clear (using cfg: GameCanvas.Config) =
    clearRect(VecI.zero(2), cfg.size)
  
object GameCanvas:
  
  case class Config (
    size: VecI,
    scale: Int,
    offset: VecI,
  )