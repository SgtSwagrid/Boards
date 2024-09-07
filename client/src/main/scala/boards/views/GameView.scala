package boards.views

import boards.algebra.PieceSet.Diff
import boards.algebra.Rule
import boards.components.*
import boards.games.Chess
import boards.graphics.Scene.{Input, InputType, Tile}
import boards.graphics.{Scene, Texture}
import boards.protocol.GameProtocol.*
import util.math.Vec.{VecF, VecI}

import scala.scalajs.js.annotation.JSExportTopLevel
import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.api.features.unitArrows
import io.laminext.websocket.*
import io.laminext.websocket.circe.*
import io.circe.*
import io.circe.syntax.*
import io.circe.parser.*
import io.circe.generic.auto.*
import org.scalajs.dom
import org.scalajs.dom.KeyCode.{H, W}
import org.scalajs.dom.{CanvasRenderingContext2D, DOMRect, HTMLImageElement, MouseEvent, document}
import org.scalajs.dom.html.Canvas
import util.math.Metric

import scala.collection.mutable

@JSExportTopLevel("GameView")
object GameView extends View:
  println(Chess.initial(2).actions.toSeq)
  //println(Chess.initial(2).now.pieces.pieces)
  
  private val roomId: String =
    document.documentURI.split("/").last
  
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
  
  def fillRect(pos: VecI, size: VecI, style: String) =
    ctx.fillStyle = style
    ctx.fillRect(pos.x, pos.y, size.x, size.y)
    
  def clearRect(pos: VecI, size: VecI) =
    ctx.clearRect(pos.x, pos.y, size.x, size.y)
    
  def drawImage(pos: VecI, size: VecI, image: Texture) =
    loadTexture(s"/assets/images/games/${image.file}"): img =>
      ctx.drawImage(img, pos.x, pos.y, size.x, size.y)
  
  private val socket: WebSocket[Scene, GameRequest] =
    WebSocket.path(s"/game/$roomId/socket").json.build()
    
  private val scene: Signal[Scene] =
    socket.received.startWith(Scene.empty)
  
  private val canvasSize: Signal[VecI] =
    windowEvents(_.onLoad).mergeWith(windowEvents(_.onResize)).map: _ =>
      canvas.width = rect.width.toInt
      canvas.height = rect.height.toInt
      VecI(rect.width.toInt, rect.height.toInt)
    .startWith(VecI.zero)
  
  private val cursorBus: EventBus[MouseEvent] = new EventBus[MouseEvent]
  
  private val cursorPos: Signal[VecI] = cursorBus.stream
    .map(e => VecI(e.clientX.toInt, e.clientY.toInt))
    .startWith(VecI.zero)
    .map(m => VecI(m.x - rect.left.toInt, m.y - rect.top.toInt))
    
  private val scale: Signal[Int] =
    scene.combineWith(canvasSize).map: (scene, canvasSize) =>
      try Math.min(canvasSize.x * scene.board.height, canvasSize.y * scene.board.width) /
        (scene.board.width * scene.board.height)
      catch case _ => 0
    
  private val hover: Signal[Option[Tile]] =
    scene.combineWith(canvasSize, cursorPos, scale)
      .map: (scene, canvasSize, cursorPos, scale) =>
        scene.board.labels.find: tile =>
          tile.pos <= cursorPos && cursorPos <= (tile.pos + VecI(scale, scale))
  
  private enum MouseButton:
    case Left, Right, Middle
  
  private enum MouseAction:
    case Down, Up
  
  private case class ClickEvent(button: MouseButton, action: MouseAction)
  
  private val clickBus: EventBus[ClickEvent] = new EventBus[ClickEvent]
  
  private val dragged: Signal[Option[Tile]] =
    clickBus.stream.filter(_.button == MouseButton.Left).map(_.action)
      .withCurrentValueOf(hover).map:
        case (MouseAction.Down, hover) => hover
        case (MouseAction.Up, _) => None
      .startWith(None)
    
  private val tentativeInput: Signal[Option[Input]] =
    dragged.combineWith(hover).map: (from, to) =>
      from.zip(to).flatMap: (from, to) =>
        from.inputs.find: input =>
          input.inputType match
            case InputType.Click(v) => from == v && to == v
            case InputType.Drag(u, v) => from == u && to == v
            case _ => false
            
  private val diff: Signal[Map[VecI, Option[Diff]]] =
    scene.combineWith(tentativeInput).map: (scene, input) =>
      val home = scene.board.labels.map(t => t.pos -> None).toMap
      input match
        case None => home
        case Some(input) =>
          home ++ input.diff.map(d => d.target -> Some(d))
          
  private val targetPiecePositions: Signal[Map[VecI, VecI]] =
    diff.combineWith(scale).map: (target, scale) =>
      target.map:
        case pos -> Some(Diff.Relocate(_, to)) => pos -> (to * scale)
        case pos -> _ => pos -> (pos * scale)
  
  private val currentPiecePositions: Signal[Map[VecI, VecI]] =
    scene.combineWith(scale).flatMapSwitch: (scene, scale) =>
      
      val initial = scene.board.labels
        .filter(_.piece.nonEmpty)
        .map(t => t.pos -> t.pos * scale).toMap
      
      EventStream.periodic(20)
        .withCurrentValueOf(targetPiecePositions).map((_, p) => p)
        .scanLeft(initial): (positions, targets) =>
          positions.map: (home, current) =>
            val target = targets(home)
            home -> (
              if current.dist(target)(using Metric.Chebyshev) < 100 then target
              else current + ((target - current) / 100)
            )
    
  private def draw(scene: Scene, positions: Map[VecI, VecI], canvasSize: VecI, cursorPos: VecI, scale: Int) =
    
    val square = VecI(scale, scale)
    
    val (width, height) = (scale * scene.board.width, scale * scene.board.height)
    val (h_offset, v_offset) = ((canvasSize.x - width) / 2, (canvasSize.y - height) / 2)
    
    clearRect(VecI.zero, canvasSize)
    
    for (tile <- scene.board.labels) do
      val pos = VecI (
        tile.pos.x * scale + h_offset,
        canvasSize.y - (tile.pos.y + 1) * scale - v_offset
      )
      
      if pos <= cursorPos && cursorPos <= (pos + square) then
        println(tile)
      
      val colour = if pos <= cursorPos && cursorPos <= (pos + square) then
        tile.colour.darken(20) else tile.colour
      fillRect(pos, square, colour.hexString)
      tile.piece.foreach(texture => drawImage(positions(tile.pos), square, texture))
  
  private val updateStream: EventStream[(Scene, Map[VecI, VecI], VecI, VecI, Int)] =
    scene.changes.mergeWith(canvasSize.changes, cursorPos.changes)
      .withCurrentValueOf(scene, currentPiecePositions, canvasSize, cursorPos, scale)
      .map((_, s, pos, cs, cp, sc) => (s, pos, cs, cp, sc))
  
  def content = div (
    socket.connect,
    onMouseMove --> cursorBus.writer,
    onMouseDown.map(e => ClickEvent(MouseButton.Left, MouseAction.Down)) --> clickBus,
    onMouseUp.map(e => ClickEvent(MouseButton.Left, MouseAction.Up)) --> clickBus,
    updateStream --> draw.tupled,
    Navbar(),
    div (
      position("absolute"),
      top("120px"), left("50px"), right("50px"), bottom("130px"),
      canvasTag (
        idAttr("game"),
        width("100%"),
        height("100%"),
      ),
    ),
    Footer()
  )