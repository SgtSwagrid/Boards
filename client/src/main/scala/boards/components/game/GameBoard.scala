package boards.components.game

import boards.graphics.Scene
import boards.graphics.Scene.{Choice, PieceData, Tile}
import boards.imports.laminar.HtmlProp
import boards.math.region.Metric
import boards.protocol.GameProtocol.GameRequest
import boards.protocol.Room.Player
import boards.views.GameView.{scene, sceneBus, socket}
import com.raquo.laminar.codecs.StringAsIsCodec
import com.raquo.laminar.modifiers.RenderableText
import io.circe.Decoder.state
import jdk.jfr.internal.event.EventWriter
import org.scalajs.dom.Audio

import java.awt.Canvas
import java.time.temporal.TemporalQueries.offset
import scala.scalajs.js.annotation.JSExportTopLevel
//import boards.imports.laminar.{*, given}
import boards.imports.circe.{*, given}
import boards.imports.math.{*, given}
import boards.imports.games.{*, given}
import com.raquo.laminar.nodes.ChildNode
import org.scalajs.dom.{CanvasRenderingContext2D, DOMRect, HTMLImageElement, MouseEvent}

import scala.collection.mutable

import boards.util.extensions.SequenceOps.*

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
  
  val PIECE_MOVEMENT_SPEED = 0.4F
  val PIECE_GROWTH_SPEED = 0.2F
  
  val SELECTED_TILE_COLOUR = Colour.British.RiseNShine
  val UPDATED_TILE_COLOUR = Colour.British.Naval
  val TILE_HIGHLIGHT_OPACITY = 0.75F
  val HOVERED_TILE_DARKEN = 15
  
  val SMALL_CIRCLE_SIZE = 0.1F
  val LARGE_CIRCLE_SIZE = 0.4F
  val CROSS_SIZE = 0.15F
  val CIRCLE_COLOUR = Colour.British.LynxWhite
  val CROSS_COLOUR = Colour.British.NasturcianFlower
  val HINT_OPACITY = 0.6F
  
  private val scene = sceneBus.events.startWith(Scene.empty)
  
  private val canvas = GameCanvas(scene)
      
  private object Mouse:
    
    enum Button:
      case Left, Right, Middle
    
    enum Action:
      case Down, Up
      
    case class Click(button: Mouse.Button, action: Mouse.Action)
    
    val moves: EventBus[MouseEvent] = new EventBus
    val clicks: EventBus[Mouse.Click] = new EventBus
    val leave: EventBus[MouseEvent] = new EventBus
    
    def clicksOf(button: Mouse.Button): EventStream[Mouse.Action] =
      clicks.stream.delay(1).filter(_.button == button).map(_.action)
    val leftClicks = clicksOf(Mouse.Button.Left)
    
    /** The current position of the cursor in pixels relative to the canvas. */
    val cursorPos: Signal[VecI] = moves.stream
      .map(e => VecI(e.clientX.toInt, e.clientY.toInt))
      .startWith(VecI.zero(2))
      .map(m => VecI(m.x - canvas.bounds.left.toInt, m.y - canvas.bounds.top.toInt))
  
  /** The tile over which the cursor is currently hovering. */
  private val hover: Signal[Option[Tile]] =
    Signal.combine(scene, canvas.config, Mouse.cursorPos).map: (scene, cfg, cursorPos) =>
      scene.board.label(canvas.canvasToGameCoords(cursorPos)(using cfg))
  
  private val clicked: Signal[Option[Tile]] =
    Mouse.leftClicks
      .withCurrentValueOf(hover).map(Right.apply)
      .mergeWith(hover.changes.distinct.map(Left.apply))
      .collect:
        case Right(Mouse.Action.Down, Some(tile)) => Some(tile)
        case _ => None
      .startWith(None)
  
  /** The board tile currently being dragged from. */
  private val dragged: Signal[Option[Tile]] =
    EventStream.merge(Mouse.leftClicks, Mouse.leave.stream)
      .withCurrentValueOf(hover).map:
        case (Mouse.Action.Down, Some(hover)) => Some(hover)
        case _ => None
      .startWith(None)
  
  /** The action the user might be about to take. */
  private val provisionalInput: Signal[Option[Choice[?]]] =
  
    val click: Signal[Option[Choice[Input.Click]]] =
      Signal.combine(scene, clicked).map: (scene, clicked) =>
        for
          clicked <- clicked
          choices <- scene.clicksByOrigin.get(clicked.position)
          choice <- choices.headOption
        yield choice
    
    val drag: Signal[Option[Choice[Input.Drag]]] =
      Signal.combine(scene, dragged, canvas.config, Mouse.cursorPos).map: (scene, dragged, cfg, cursorPos) =>
        given GameCanvas.Config = cfg
        dragged.filter(_ => canvas.inBounds(cursorPos)).flatMap: dragged =>
          val choices = scene.dragsByOrigin.getOrElse(dragged.position, Seq.empty)
          val target = (choices.flatMap(_.input.to.positions) :+ dragged.position)
            .minBy: tilePos =>
              Metric.EuclideanSquared.dist(canvas.gameToCanvasCenterCoords(tilePos), cursorPos)
          choices.find: choice =>
            choice.input.to.contains(target)
      
    Signal.combine(click, drag).map(_.orElse(_))
  
  private val inputStream: EventStream[Choice[?]] =
    Mouse.clicks.stream.filter(_.action == Mouse.Action.Up)
      .withCurrentValueOf(provisionalInput)
      .collect { case (_, Some(choice)) => choice }
  
  private val pieceData: Signal[Map[PieceId, PieceData]] =
    Signal.combine(scene, provisionalInput).map:
      case (_, Some(input)) => input.result.piecesById
      case (scene, None) => scene.piecesById
  
  private case class PieceState (
    pieceId: PieceId,
    actualPos: VecF,
    targetPos: VecF,
    actualSize: Float,
    isAnimating: Boolean,
    isMoving: Boolean,
    texture: Texture,
  )
  
  private val pieceStates: Signal[Map[PieceId, PieceState]] =
    canvas.config.flatMapSwitch { cfg =>
      given GameCanvas.Config = cfg
      
      EventStream.periodic(5)
        .withCurrentValueOf(pieceData).map((_, s) => s)
        .scanLeft(Map.empty[PieceId, PieceState]) { (previous, pieceState) =>
          
          // Pieces which previously existed and still exist.
          val existing = (previous.keySet & pieceState.keySet)
            .map(id => (previous(id), pieceState(id))).map: (previous, pieceState) =>
              val target: VecF = canvas.gameToCanvasCenterCoords(pieceState.position)
              val isMoving = !(target - previous.actualPos).toVecI.isZero
              previous.pieceId -> PieceState (
                previous.pieceId,
                previous.actualPos + ((target - previous.actualPos) * PIECE_MOVEMENT_SPEED),
                target,
                Math.min(previous.actualSize + (cfg.scale - previous.actualSize) * PIECE_GROWTH_SPEED, cfg.scale),
                isAnimating = isMoving || previous.actualSize < cfg.scale,
                isMoving = isMoving,
                pieceState.texture,
              )
            .toMap
          
          // Pieces which were newly created.
          val created = (pieceState.keySet -- previous.keySet)
            .map(pieceState.apply).map: pieceState =>
              pieceState.pieceId -> PieceState (
                pieceState.pieceId,
                canvas.gameToCanvasCenterCoords(pieceState.position),
                canvas.gameToCanvasCenterCoords(pieceState.position),
                0.0F,
                isAnimating = true,
                isMoving = false,
                pieceState.texture,
              )
            .toMap
          
          // Pieces which were recently deleted.
          val deleted = (previous.keySet -- pieceState.keySet)
            .map(previous.apply).map: previous =>
              previous.pieceId -> PieceState (
                previous.pieceId,
                previous.actualPos,
                previous.actualPos,
                previous.actualSize * (1.0F - PIECE_GROWTH_SPEED),
                isAnimating = true,
                isMoving = false,
                previous.texture,
              )
            .filter((_, p) => p.actualSize.toInt > 0)
            .toMap
          
          existing ++ created ++ deleted
        }
    }
  
  private case class PieceSprite (
    position: VecI,
    size: VecI,
    texture: Texture,
  )
  
  private val pieceSprites: Signal[Seq[PieceSprite]] =
    pieceStates.map(_.values.toSeq
      .sortBy(_.isMoving)
      .sortBy(_.actualSize)
      .map: piece =>
        val size = VecI(piece.actualSize.toInt, piece.actualSize.toInt)
        PieceSprite (
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
    provisionalInput: Option[Choice[?]],
    cfg: GameCanvas.Config,
  ) =
    
    given GameCanvas.Config = cfg
    
    val square = VecI.fill(2)(cfg.scale)
    
    canvas.clearRect(VecI.zero(2), cfg.size)
    
    for tile <- scene.board.labels do
      val pos = canvas.gameToCanvasCornerCoords(tile.position)
      val baseColour =
        if dragged.exists(_.position == tile.position)
        then tile.colour.mix(SELECTED_TILE_COLOUR, TILE_HIGHLIGHT_OPACITY)
        else if scene.diff.contains(tile.position)
        then tile.colour.mix(UPDATED_TILE_COLOUR, TILE_HIGHLIGHT_OPACITY)
        else tile.colour
      val colour = if hover.exists(_.position == tile.position)
        then baseColour.darken(HOVERED_TILE_DARKEN) else baseColour
      canvas.fillRect(pos, square, colour)
      
    if provisionalInput.isEmpty then
      for origin <- scene.clicksByOrigin.keys do
        val pos = canvas.gameToCanvasCenterCoords(origin)
        canvas.fillCircle(pos, cfg.scale * SMALL_CIRCLE_SIZE, CIRCLE_COLOUR, HINT_OPACITY)
      
    if dragged.isEmpty then
      for origin <- scene.dragsByOrigin.keys do
        val pos = canvas.gameToCanvasCenterCoords(origin)
        canvas.fillCircle(pos, cfg.scale * LARGE_CIRCLE_SIZE, CIRCLE_COLOUR, HINT_OPACITY)
    
    for piece <- pieces do
      canvas.drawImage(piece.position, piece.size, piece.texture)

    dragged.foreach: dragged =>
      for choice <- scene.dragsByOrigin.getOrElse(dragged.position, Seq.empty) do
        if !provisionalInput.map(_.input).collect{ case d: Input.Drag => d }.exists(_.to == choice.input.to) then
          choice.input.to.asVec.foreach: to =>
            val pos = canvas.gameToCanvasCenterCoords(to)
            if scene.piecesByPos.contains(to)
            then canvas.drawCross(pos, cfg.scale * CROSS_SIZE, Colour.British.NasturcianFlower,
              cfg.scale * CROSS_SIZE / 3.0F, HINT_OPACITY)
            else canvas.fillCircle(pos, cfg.scale * SMALL_CIRCLE_SIZE, CIRCLE_COLOUR, HINT_OPACITY)
  
  def apply: HtmlElement = div (
    
    width("100%"), height("100%"),
    
    canvas.apply,
    
    // Redraw the board when something changes.
    Signal.combine(scene, pieceSprites, hover, dragged, provisionalInput, canvas.config).changes --> draw.tupled,
    
    // Respond to mouse input.
    onMouseMove --> Mouse.moves,
    onMouseLeave --> Mouse.leave,
    onMouseDown.map(e => Mouse.Click(Mouse.Button.Left, Mouse.Action.Down)) --> Mouse.clicks,
    onMouseUp.map(e => Mouse.Click(Mouse.Button.Left, Mouse.Action.Up)) --> Mouse.clicks,
    
    // Tell the server when the user takes an action.
    inputStream.map(input => input.result) --> sceneBus.writer,
    inputStream.map(input => GameRequest.TakeAction(input.choiceId)) --> response,
  )