package boards.components.game

import boards.graphics.Scene
import boards.graphics.Scene.{Input, PieceData, Tile}
import boards.imports.laminar.HtmlProp
import boards.protocol.GameProtocol.{GameRequest, Player, RegisteredParticipant, UnregisteredParticipant, Status}
import boards.views.GameView.{scene, sceneBus, socket}
import com.raquo.laminar.codecs.StringAsIsCodec
import com.raquo.laminar.modifiers.RenderableText
import io.circe.Decoder.state
import jdk.jfr.internal.event.EventWriter
import org.scalajs.dom.Audio

import scala.scalajs.js.annotation.JSExportTopLevel
//import boards.imports.laminar.{*, given}
import boards.imports.circe.{*, given}
import boards.imports.math.{*, given}
import boards.imports.games.{*, given}
import com.raquo.laminar.nodes.ChildNode
import org.scalajs.dom.{CanvasRenderingContext2D, DOMRect, HTMLImageElement, MouseEvent}

import scala.collection.mutable

import boards.util.Extensions.*

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

import org.scalajs.dom.{
  document,
  window,
  html,
  KeyCode,
}

import org.scalajs.dom.html.Canvas

import com.raquo.laminar.api.L.{*, given}

import com.raquo.laminar.nodes.{ReactiveElement, ReactiveHtmlElement, ReactiveSvgElement}
import com.raquo.laminar.tags.HtmlTag

import io.laminext.syntax.core.*

import io.laminext.fetch.circe.{Fetch, jsonRequestBody}
import io.laminext.websocket.circe.{WebSocket, WebSocketEvent, WebSocketError}
import io.laminext.fetch.circe.fetchEventStreamBuilderSyntaxCirce
import io.laminext.websocket.circe.webSocketReceiveBuilderSyntax

import boards.components.{
  ExpandingButton,
  Footer,
  InputField,
  Navbar,
  SVG,
  Tabs,
}

class GameBoard(sceneBus: EventBus[Scene], response: Observer[GameRequest]):
  
  private val scene = sceneBus.events.startWith(Scene.empty)
  
  private val textures: mutable.Map[String, HTMLImageElement] = mutable.Map.empty
  private def loadTexture(src: String)(onload: HTMLImageElement => Unit): Unit =
    textures.get(src) match
      case Some(texture) => onload(texture)
      case None =>
        val img = document.createElement("img").asInstanceOf[HTMLImageElement]
        img.src = src
        img.onload = _ =>
          textures += (src -> img)
          onload(img)
  
  private lazy val canvas = document.getElementById("game").asInstanceOf[Canvas]
  private lazy val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
  private def rect: DOMRect = canvas.getBoundingClientRect()
  
  val sound = Audio("/assets/audio/place.mp3")
  
  def fillRect(pos: VecI, size: VecI, colour: Colour, alpha: Float = 1.0) =
    ctx.fillStyle = colour.hexString
    ctx.globalAlpha = alpha
    ctx.fillRect(pos.x, pos.y, size.x, size.y)
    
  def fillCircle(centre: VecI, radius: Float, colour: Colour, alpha: Float = 1.0) =
    ctx.fillStyle = colour.hexString
    ctx.globalAlpha = alpha
    ctx.beginPath()
    ctx.arc(centre.x, centre.y, radius, 0, 2 * Math.PI)
    ctx.fill()
  
  def drawCircle(centre: VecI, radius: Float, colour: Colour, thickness: Float, alpha: Float = 1.0) =
    ctx.strokeStyle = colour.hexString
    ctx.lineWidth = thickness
    ctx.globalAlpha = alpha
    ctx.beginPath()
    ctx.arc(centre.x, centre.y, radius, 0, 2 * Math.PI)
    ctx.stroke()
    
  def drawCross(centre: VecI, radius: Float, colour: Colour, thickness: Float, alpha: Float = 1.0F) =
    ctx.strokeStyle = colour.hexString
    ctx.lineWidth = thickness
    ctx.globalAlpha = alpha
    ctx.beginPath()
    ctx.moveTo(centre.x - radius, centre.y - radius)
    ctx.lineTo(centre.x + radius, centre.y + radius)
    ctx.moveTo(centre.x - radius, centre.y + radius)
    ctx.lineTo(centre.x + radius, centre.y - radius)
    ctx.stroke()
  
  def clearRect(pos: VecI, size: VecI) =
    ctx.clearRect(pos.x, pos.y, size.x, size.y)
  
  def drawImage(pos: VecI, size: VecI, image: Texture, alpha: Float = 1.0F) =
    loadTexture(s"/assets/images/games/${image.file}"): img =>
      ctx.globalAlpha = alpha
      ctx.drawImage(img, pos.x, pos.y, size.x, size.y)
  
  private val canvasSize: Signal[VecI] =
    EventStream.merge(windowEvents(_.onLoad), windowEvents(_.onResize)).map: _ =>
      canvas.width = rect.width.toInt
      canvas.height = rect.height.toInt
      canvas.oncontextmenu = e =>
        e.preventDefault()
        e.stopPropagation()
      VecI(rect.width.toInt, rect.height.toInt)
    .startWith(VecI.zero)
  
  /** The size in pixels of a single tile from the game board. */
  private val scale: Signal[Int] =
    Signal.combine(scene, canvasSize).map: (scene, canvasSize) =>
      try (canvasSize * scene.board.size).toSeq.min / scene.board.size.product
      catch case _ => 0
  
  /** The offset in pixels from the corner of the canvas to the corner of the game board. */
  private val offset: Signal[VecI] =
    Signal.combine(scene, canvasSize, scale).map: (scene, canvasSize, scale) =>
      (canvasSize - (scene.board.size * scale)) / 2
  
  /** Records all cursor movement events. */
  private val cursorBus: EventBus[MouseEvent] = new EventBus[MouseEvent]
  
  /** The current position of the cursor in pixels relative to the canvas. */
  private val cursorPos: Signal[VecI] = cursorBus.stream
    .map(e => VecI(e.clientX.toInt, e.clientY.toInt))
    .startWith(VecI.zero)
    .map(m => VecI(m.x - rect.left.toInt, m.y - rect.top.toInt))
  
  private case class CanvasState(
    canvasSize: VecI,
    scale: Int,
    offset: VecI,
  )
  
  private val config: Signal[CanvasState] =
    Signal.combine(canvasSize, scale, offset).map(CanvasState.apply.tupled).distinct
  
  private def canvasToGameCoords(pos: VecI, cfg: CanvasState): VecI =
    try (pos.flipY + cfg.canvasSize.projY - cfg.offset) / cfg.scale
    catch case _ => VecI.zero(2)
  
  /** Get the position of the top-left corner of a tile in canvas (pixel) coordinates. */
  private def gameToCanvasCornerCoords(pos: VecI, cfg: CanvasState): VecI =
    (pos * cfg.scale + cfg.offset).flipY + cfg.canvasSize.projY - VecI(0, cfg.scale)
  
  /** Get the position of the center of a tile in canvas (pixel) coordinates. */
  private def gameToCanvasCenterCoords(pos: VecI, cfg: CanvasState): VecI =
    gameToCanvasCornerCoords(pos, cfg) + VecI.fill(2)(cfg.scale / 2)
  
  private def inBounds(pos: VecI, cfg: CanvasState): Boolean =
    pos >= cfg.offset && pos <= (cfg.canvasSize - cfg.offset)
  
  /** The tile over which the cursor is currently hovering. */
  private val hover: Signal[Option[Tile]] =
    Signal.combine(scene, config, cursorPos).map: (scene, cfg, cursorPos) =>
      scene.board.label(canvasToGameCoords(cursorPos, cfg))
  
  private enum MouseButton:
    
    case Left, Right, Middle
  
  private enum MouseAction:
    
    case Down, Up
  
  private case class ClickEvent(button: MouseButton, action: MouseAction)
  
  private val clickBus: EventBus[ClickEvent] = new EventBus[ClickEvent]
  
  /** The board tile currently being dragged from. */
  private val dragged: Signal[Option[Tile]] =
    clickBus.stream.delay(1).filter(_.button == MouseButton.Left).map(_.action)
      .withCurrentValueOf(hover).map:
        case (MouseAction.Down, hover) => hover
        case (MouseAction.Up, _) => None
      .startWith(None)
  
  /** The action the user might be about to take. */
  private val tentativeInput: Signal[Option[Input]] =
    Signal.combine(scene, dragged, config, cursorPos).map: (scene, dragged, cfg, cursorPos) =>
      dragged.filter(_ => inBounds(cursorPos, cfg)).flatMap: dragged =>
        val inputs = scene.inputsByOrigin.getOrElse(dragged.pos, Seq.empty)
        val target = (inputs.map(_.to) :+ dragged.pos)
          .minBy: pos =>
            Metric.EuclideanSquared.dist(gameToCanvasCenterCoords(pos, cfg), cursorPos)
        inputs.find: input =>
          input.to == target
  
  private val inputStream: EventStream[Input] =
    clickBus.stream.filter(_.action == MouseAction.Up)
      .withCurrentValueOf(tentativeInput)
      .filter((_, input) => input.isDefined)
      .map((_, input) => input.get)
  
  private val pieceState: Signal[Map[Int, PieceData]] =
    Signal.combine(scene, tentativeInput).map:
      case (_, Some(input)) => input.result.piecesById
      case (scene, None) => scene.piecesById
  
  private case class PieceState (
    pieceId: Int,
    actualPos: VecF,
    targetPos: VecF,
    actualSize: Float,
    isMoving: Boolean,
    texture: Texture,
  )
  
  private case class PieceSprite (
    position: VecI,
    size: VecI,
    texture: Texture,
  )
  
  private val pieces: Signal[Seq[PieceSprite]] =
    config.flatMapSwitch { cfg =>
      
      EventStream.periodic(5)
        .withCurrentValueOf(pieceState).map((_, s) => s)
        .scanLeft(Map.empty[Int, PieceState]) { (previous, pieceState) =>
          
          // Pieces which previously existed and still exist.
          val existing = (previous.keySet & pieceState.keySet)
            .map(id => (previous(id), pieceState(id))).map: (previous, pieceState) =>
              val target: VecF = gameToCanvasCenterCoords(pieceState.pos, cfg)
              previous.pieceId -> PieceState(
                previous.pieceId,
                previous.actualPos + ((target - previous.actualPos) * 0.2F),
                target,
                Math.min(previous.actualSize + (cfg.scale - previous.actualSize) * 0.1F, cfg.scale),
                !(target - previous.actualPos).toVecI.isZero,
                pieceState.texture,
              )
            .toMap
          
          // Pieces which were recently deleted.
          val created = (previous.keySet -- pieceState.keySet)
            .map(previous.apply).map: previous =>
              previous.pieceId -> PieceState(
                previous.pieceId,
                previous.actualPos,
                previous.actualPos,
                previous.actualSize * 0.9F,
                false,
                previous.texture,
              )
            .filter((_, p) => p.actualSize.toInt > 0)
            .toMap
          
          // Pieces which were newly created.
          val deleted = (pieceState.keySet -- previous.keySet)
            .map(pieceState.apply).map: pieceState =>
              pieceState.pieceId -> PieceState(
                pieceState.pieceId,
                gameToCanvasCenterCoords(pieceState.pos, cfg),
                gameToCanvasCenterCoords(pieceState.pos, cfg),
                0.0F,
                false,
                pieceState.texture,
              )
            .toMap
          
          existing ++ created ++ deleted
        }
    }.map(_.values.toSeq
      .sortBy(_.isMoving)
      .sortBy(_.actualSize)
      .map: piece =>
        val size = VecI(piece.actualSize.toInt, piece.actualSize.toInt)
        PieceSprite(
          (piece.actualPos - (size / 2)).toVecI,
          size,
          piece.texture,
        )
    )
  
  private def draw (
    scene: Scene,
    pieces: Seq[PieceSprite],
    hover: Option[Tile],
    dragged: Option[Tile],
    tentativeInput: Option[Input],
    cfg: CanvasState,
  ) =
    
    val square = VecI.fill(2)(cfg.scale)
    
    clearRect(VecI.zero, cfg.canvasSize)
    
    for tile <- scene.board.labels do
      val pos = gameToCanvasCornerCoords(tile.pos, cfg)
      val baseColour =
        if dragged.exists(_.pos == tile.pos) then tile.colour.mix(Colour.British.RiseNShine, 0.75F)
        else if scene.diffSet.contains(tile.pos) then tile.colour.mix(Colour.British.Naval, 0.75F)
        else tile.colour
      val colour = if hover.exists(_.pos == tile.pos) then baseColour.darken(15) else baseColour
      fillRect(pos, square, colour)
      
    if dragged.isEmpty then
      for origin <- scene.inputsByOrigin.keys do
        val pos = gameToCanvasCenterCoords(origin, cfg)
        fillCircle(pos, cfg.scale * 0.4F, Colour.British.LynxWhite, 0.4F)
    
    for piece <- pieces do
      drawImage(piece.position, piece.size, piece.texture)

    dragged.foreach: dragged =>
      for input <- scene.inputsByOrigin.getOrElse(dragged.pos, Seq.empty) do
        if !tentativeInput.exists(_.to == input.to) then
          val pos = gameToCanvasCenterCoords(input.to, cfg)
          if scene.piecesByPos.contains(input.to)
          then drawCross(pos, cfg.scale * 0.15F, Colour.British.NasturcianFlower, cfg.scale * 0.05F, 0.6F)
          else fillCircle(pos, cfg.scale * 0.1F, Colour.British.LynxWhite, 0.6F)
  
  def apply: HtmlElement = canvasTag (
    
    idAttr("game"),
    width("100%"),
    height("100%"),
    
    // Redraw the board when something changes.
    Signal.combine(scene, pieces, hover, dragged, tentativeInput, config).changes --> draw.tupled,
    
    // Respond to mouse input.
    onMouseMove --> cursorBus.writer,
    onMouseDown.map(e => ClickEvent(MouseButton.Left, MouseAction.Down)) --> clickBus,
    onMouseUp.map(e => ClickEvent(MouseButton.Left, MouseAction.Up)) --> clickBus,
    
    // Tell the server when the user takes an action.
    inputStream.map(input => input.result) --> sceneBus.writer,
    inputStream.map(input => GameRequest.TakeAction(input.actionHash)) --> response,
    
    // Play a sound when a piece moves.
    scene.map(_.pieces).changes.distinct --> {_ => sound.play()},
  )