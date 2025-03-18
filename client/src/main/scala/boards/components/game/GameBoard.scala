package boards.components.game

import boards.graphics.{Colour, Scene, Texture}
import boards.graphics.Scene.{Choice, PieceData}
import boards.math.vector.Embedding.Tile
import boards.math.vector.{Bounds, Embedding, Metric, Vec}
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
import boards.components.{ExpandingButton, Footer, InputField, Navbar, SVG, Tabs}
import boards.dsl.pieces.PieceRef.PieceId
import boards.dsl.pieces.PieceType.PieceAppearance
import boards.dsl.rules.Input
import boards.math.vector.Vec.{VecF, VecI, given}
import boards.math.algebra.Algebra.{*, given}
import boards.math.vector.Bounds.{BoundsF, BoundsI}
import boards.math.Conversions.*

/** The primary component of the [[GameView]], containing the game board itself.
  * Responsible for taking all player input and state rendering.
  * @param sceneBus A bus for receiving and publishing changes to the current scene/game state.
  * @param response A channel for sending messages back to the server.
  */
class GameBoard (
  sceneBus: EventBus[Scene],
  response: Observer[GameRequest],
):
  
  val PIECE_MOVEMENT_SPEED = 0.15F
  val PIECE_GROWTH_SPEED = 0.06F
  
  val SELECTED_TILE_COLOUR = Colour.British.RiseNShine
  val UPDATED_TILE_COLOUR = Colour.British.Naval
  val TILE_HIGHLIGHT_OPACITY = 192
  val HOVERED_TILE_DARKEN = 15
  
  val SMALL_CIRCLE_SIZE = 0.2F
  val LARGE_CIRCLE_SIZE = 0.8F
  val CROSS_SIZE = 0.15F
  val CIRCLE_COLOUR = Colour.British.LynxWhite
  val CROSS_COLOUR = Colour.British.NasturcianFlower
  val HINT_OPACITY = 150
  
  /** The current version of the scene,
    * which contains all necessary information to render the current state of the game.
    * Can be modified by updates from the server and pre-emptively by local actions.
    */
  private val scene = sceneBus.events.startWith(Scene.empty)
  
  /** The HTML canvas into which the current state of the game board should be rendered. */
  private val canvas = ReactiveCanvas("game")
  
  case class ScaledScene (
    scene: Scene,
    canvas: BoundsF,
  ):
    val board: Embedding = scene.board.flipY.fitTo(canvas)
    export scene.{board => _, *}
    export board.{toEmbeddedSpaceOpt, toEmbeddedSpace, toLogicalSpaceOpt, toLogicalSpace}
      
  private val scaledScene: Signal[ScaledScene] =
    Signal.combine(scene, canvas.bounds).map(ScaledScene.apply.tupled)
  
  /** A collection of events relating to mouse buttons and cursor movement. */
  private object Mouse:
    
    /** The mouse button clicked or released in a [[Click]] event. */
    enum Button:
      case Left, Right, Middle
    
    /** The action taken by a mouse button in a [[Click]] event. */
    enum Action:
      case Down, Up
    
    /** An event describing a mouse button press or release.
      * @param button The mouse button in question.
      * @param action Whether button was initially pressed or eventually released.
      */
    case class Click (button: Mouse.Button, action: Mouse.Action)
    
    /** An event stream of all cursor movements. */
    val moves: EventBus[MouseEvent] = new EventBus
    /** An event stream of all mouse button presses. */
    val clicks: EventBus[Mouse.Click] = new EventBus
    /** An event stream of every time the cursor leaves the game board. */
    val leave: EventBus[MouseEvent] = new EventBus
    
    /** A stream consisting of all clicks of the specified mouse button. */
    def clicksOf (button: Mouse.Button): EventStream[Mouse.Action] =
      clicks.stream.delay(1).filter(_.button == button).map(_.action)
    val leftClicks = clicksOf(Mouse.Button.Left)
    
    /** The current position of the cursor in pixels relative to the canvas. */
    val cursorPos: Signal[VecI] = moves.stream.mergeWith(leave.stream)
      .map(e => VecI(e.clientX.toInt, e.clientY.toInt))
      .startWith(VecI.zero(2))
      .map(m => VecI(m.x - canvas.rect.left.toInt, m.y - canvas.rect.top.toInt))
      .distinct
  
  /** The tile over which the cursor is currently hovering. */
  private val hover: Signal[Option[Tile]] =
    Signal.combine(scaledScene, Mouse.cursorPos).map: (scene, cursorPos) =>
      scene.toLogicalSpaceOpt(cursorPos.toVecF).map(scene.board.tile)
  
  /** The tile which is currently being clicked, if any.
    * A tile is considered clicked if: <br />
    *   (a) The cursor is currently hovering over this tile, <br />
    *   (b) The left mouse button is currently down, and <br />
    *   (c) The cursor hasn't left this tile since the left mouse button was first pressed.
    */
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
      .distinct
  
  /** The action the user might be about to take, if any.
    * Exists when the user is in the process of taking an action,
    * but hasn't yet confirmed it by releasing the mouse button.
    * Used to show a preview of the result, and to actually trigger the action on mouse release.
    */
  private val provisionalInput: Signal[Option[Choice[?]]] = {
    
    /** The click input the user might be about to take. */
    val click: Signal[Option[Choice[Input.Click]]] =
      Signal.combine(scaledScene, clicked).map: (scene, clicked) =>
        for
          clicked <- clicked
            choices <- scene.clicksByOrigin.get(clicked.logicalPosition)
            choice <- choices.headOption
        yield choice
    
    /** The drag input the user might be about to take. */
    val drag: Signal[Option[Choice[Input.Drag]]] =
      Signal.combine(scaledScene, dragged)
        .flatMapSwitch { (scene, dragged) =>
          dragged.map { dragged =>
            val choices = scene.dragsByOrigin.getOrElse(dragged.logicalPosition, Seq.empty)
            val targets = choices
              .flatMap(c => c.input.to.positions.map(v => (v, Some(c)))) :+ (dragged
              .logicalPosition, None)
            Mouse.cursorPos.map { cursorPos =>
              targets.minBy { (tilePos, _) =>
                Metric.EuclideanSquared
                  .dist(scene.toEmbeddedSpace(tilePos).centre.toVecI, cursorPos)
              }.apply(1)
            }
          }.getOrElse(Val(None))
        }
    
    Signal.combine(click, drag).map(_.orElse(_))
  }
  
  /** The stream of all confirmed inputs by players on this device.
    * All such events are triggered locally and are immediately sent to the server.
    */
  private val inputStream: EventStream[Choice[?]] =
    Mouse.clicks.stream.filter(_.action == Mouse.Action.Up)
      .withCurrentValueOf(provisionalInput)
      .collect { case (_, Some(choice)) => choice }
  
  /** All currently existing pieces, indexed by ID. */
  private val provisionalScene: Signal[ScaledScene] =
    Signal.combine(scene, provisionalInput, canvas.bounds).map:
      case (_, Some(input), bounds) => ScaledScene(input.result, bounds)
      case (scene, None, bounds) => ScaledScene(scene, bounds)
  
  /** A piece as it exists in the current frame,
    * with additional animation-related metadata.
    * @param pieceId The ID of this piece.
    * @param actualCentrePos The current position of the centre of this piece, in canvas coordinates.
    * @param targetCentrePos The position where this piece should be, in canvas coordinates.
    * @param actualScale The current size length of this piece, in pixels.
    * @param isAnimating Whether this piece is currently moving or growing/shrinking.
    * @param isMoving Whether this piece is currently moving (i.e. actualPos != targetPos).
    * @param appearance The texture of this piece.
    */
  private case class PieceSprite (
    pieceId: PieceId,
    actualBounds: BoundsF,
    targetBounds: BoundsF,
    appearance: PieceAppearance,
  ):
    val actualSize: VecF = actualBounds.size
    val actualCentre: VecF = actualBounds.centre
    val targetSize: VecF = targetBounds.size
    val targetCentre: VecF = targetBounds.centre
    val isMoving: Boolean = (targetCentre - actualCentre).abs.norm >= 1.0F
    val isAnimating: Boolean = isMoving || (targetSize - actualSize).abs.norm >= 1.0F
  
  /** All currently existing pieces, with animation-related metadata. */
  private val pieceSprites: Signal[Seq[PieceSprite]] =
    EventStream.periodic(5)
      .withCurrentValueOf(provisionalScene).map((_, scene) => scene)
      .scanLeft(Map.empty[PieceId, PieceSprite]) { (previous, scene) =>
        
        /** Pieces which previously existed and still exist. */
        val existing = (previous.keySet & scene.piecesById.keySet)
          .map(id => (previous(id), scene.piecesById(id))).map: (previous, piece) =>
            val target = scene.toEmbeddedSpace(piece.position).inscribedBounds
            val isMoving = !(target.centre - previous.actualCentre).toVecI.isZero
            previous.pieceId -> PieceSprite (
              pieceId = previous.pieceId,
              actualBounds = (previous.actualBounds.extend((target.size - previous.actualSize) * PIECE_GROWTH_SPEED))
                + ((target.centre - previous.actualCentre) * PIECE_MOVEMENT_SPEED),
              targetBounds = target,
              appearance = piece.appearance,
            )
          .toMap
        
        /** Pieces which were newly created. */
        val created = (scene.piecesById.keySet -- previous.keySet)
          .map(scene.piecesById.apply).map: piece =>
            val target = scene.toEmbeddedSpace(piece.position).inscribedBounds
            piece.pieceId -> PieceSprite (
              pieceId = piece.pieceId,
              actualBounds = target.collapseToCentre,
              targetBounds = target,
              appearance = piece.appearance,
            )
          .toMap
        
        /** Pieces which were recently deleted. */
        val deleted = (previous.keySet -- scene.piecesById.keySet)
          .map(previous.apply).map: previous =>
            previous.pieceId -> PieceSprite (
              pieceId = previous.pieceId,
              actualBounds = previous.actualBounds.scaleCentred(1.0F - PIECE_GROWTH_SPEED),
              targetBounds = previous.targetBounds.collapseToCentre,
              appearance = previous.appearance,
            )
          .filter((_, p) => p.isAnimating)
          .toMap
        
        existing ++ created ++ deleted
        
      }.map(_.values.toSeq.sortBy(_.isMoving).sortBy(_.actualSize))
  
  /** A single frame to be rendered.
    * @param scene            The scene to render (current game state with player-related metadata).
    * @param pieces           All pieces on the board, with animation-related metadata.
    * @param hover            The tile over which the cursor is currently hovering, if any.
    * @param dragged          The tile which is currently being dragged from, if any.
    * @param provisionalInput The input that the user might be about to make, if any.
    *                         Used to show a preview of the result.
    */
  private case class Frame (
    scene: ScaledScene,
    pieces: Seq[PieceSprite],
    hover: Option[Tile],
    dragged: Option[Tile],
    provisionalInput: Option[Choice[?]],
  )
  
  /** Render a single frame to the board canvas.
    * This method is side-effecting.
    */
  private def draw (frame: Frame) =
    
    val Frame(scene, pieces, hover, dragged, provisionalInput) = frame
    canvas.clear()
    
    // Draw all tiles in the background.
    for tile <- scene.board.labels do
      val baseColour =
        // Highlight the piece currently being dragged, if any.
        if dragged.exists(_.logicalPosition == tile.logicalPosition)
        then tile.colour.mix(SELECTED_TILE_COLOUR, TILE_HIGHLIGHT_OPACITY.toFloat / 255.0F)
        // Highlight tiles which were modified in the previous turn.
        else if scene.diff.contains(tile.logicalPosition)
        then tile.colour.mix(UPDATED_TILE_COLOUR, TILE_HIGHLIGHT_OPACITY.toFloat / 255.0F)
        else tile.colour
      // Slightly darken the tile over which the cursor is currently hovering.
      val colour = if hover.exists(_.logicalPosition == tile.logicalPosition)
        then baseColour.darken(HOVERED_TILE_DARKEN) else baseColour
      canvas.fillPolygon(tile.embeddedPosition, colour)
      
    // If there is no provision input, highlight all possible click inputs.
    if provisionalInput.isEmpty then
      for origin <- scene.clicksByOrigin.keys do
        canvas.fillCircle (
          scene.toEmbeddedSpace(origin).inscribedBounds.scaleCentred(SMALL_CIRCLE_SIZE),
          CIRCLE_COLOUR.withAlpha(HINT_OPACITY),
        )
      
    // If no piece is currently being dragged, highlight all possible drag inputs.
    if dragged.isEmpty then
      for origin <- scene.dragsByOrigin.keys do
        canvas.fillCircle (
          scene.toEmbeddedSpace(origin).inscribedBounds.scaleCentred(LARGE_CIRCLE_SIZE),
          CIRCLE_COLOUR.withAlpha(HINT_OPACITY),
        )
    
    // Draw all pieces in the foreground.
    for piece <- pieces do
      piece.appearance match
        case PieceAppearance.Shape(shape, colour, scale) =>
          canvas.fillPolygon(shape.fitTo(piece.actualBounds.scaleCentred(scale)), colour)
        case PieceAppearance.Textured(texture) =>
          canvas.drawImage(piece.actualBounds, texture)

    // If some piece is currently being dragged, highlight all possible destinations.
    dragged.foreach: dragged =>
      for choice <- scene.dragsByOrigin.getOrElse(dragged.logicalPosition, Seq.empty) do
        if !provisionalInput.map(_.input).collect{ case d: Input.Drag => d }.exists(_.to == choice.input.to) then
          choice.input.to.asVec.foreach: to =>
            val bounds = scene.toEmbeddedSpace(to).inscribedBounds
            if scene.piecesByPos.contains(to)
            then canvas.fillCross (
              bounds.scaleCentred(CROSS_SIZE),
              bounds.size.components.min * CROSS_SIZE / 3.0F,
              CROSS_COLOUR.withAlpha(HINT_OPACITY),
            ) else canvas.fillCircle (
              bounds.scaleCentred(SMALL_CIRCLE_SIZE),
              CIRCLE_COLOUR.withAlpha(HINT_OPACITY),
            )
    
    //canvas.drawRect(scene.canvas, Colour.Red, 8.0F)
    //canvas.drawRect(scene.board.embeddedBounds, Colour.Black, 4.0F)
  
  private val frame: Var[Option[Frame]] = Var(None)
  
  window.requestAnimationFrame(renderLoop)
  private def renderLoop (t: Double): Unit =
    frame.now().foreach(draw)
    window.requestAnimationFrame(renderLoop)
  
  val apply: HtmlElement = div (
    
    width("100%"),
    height("100%"),
    
    canvas.apply,
    
    // Update the frame when something changes.
    scene.changes --> canvas.updates,
    Signal.combine(scaledScene, pieceSprites, hover, dragged, provisionalInput)
      .map(Frame.apply).map(Some.apply) --> frame,
    
    // Respond to mouse input.
    onMouseMove --> Mouse.moves,
    onMouseLeave --> Mouse.leave,
    onMouseDown.map(e => Mouse.Click(Mouse.Button.Left, Mouse.Action.Down)) --> Mouse.clicks,
    onMouseUp.map(e => Mouse.Click(Mouse.Button.Left, Mouse.Action.Up)) --> Mouse.clicks,
    
    // Tell the server when the user takes an action.
    inputStream.map(input => GameRequest.TakeAction(input.choiceId)) --> response,
    // Pre-emptively update the local scene after an action for the illusion of an instant response.
    inputStream.map(input => input.result) --> sceneBus.writer,
  )