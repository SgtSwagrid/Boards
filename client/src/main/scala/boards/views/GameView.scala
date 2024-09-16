package boards.views

import boards.graphics.Scene
import boards.graphics.Scene.{Input, PieceData, Tile}
import boards.protocol.GameProtocol.GameRequest
import boards.imports.laminar.{*, given}
import boards.imports.circe.{*, given}
import boards.imports.math.{*, given}
import boards.imports.games.{*, given}
import org.scalajs.dom.{CanvasRenderingContext2D, DOMRect, HTMLImageElement, MouseEvent}

import scala.collection.mutable

@JSExportTopLevel("GameView")
object GameView extends View:
  
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
  
  /** The size in pixels of the total available drawing space. */
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
  
  private case class CanvasState (
    canvasSize: VecI,
    scale: Int,
    offset: VecI,
  )
  
  private val config: Signal[CanvasState] =
    Signal.combine(canvasSize, scale, offset).map(CanvasState.apply.tupled)
    
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
    clickBus.stream.filter(_.button == MouseButton.Left).map(_.action)
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
  
  private val pieceState: Signal[Map[Int, PieceData]] =
    Signal.combine(scene, tentativeInput).map:
      case (_, Some(input)) => input.result
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
              previous.pieceId -> PieceState (
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
              previous.pieceId -> PieceState (
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
              pieceState.pieceId -> PieceState (
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
    }.map(_.values.toSeq.sortBy(_.isMoving).map: piece =>
      val size = VecI(piece.actualSize.toInt, piece.actualSize.toInt)
      PieceSprite (
        (piece.actualPos - (size / 2)).toVecI,
        size,
        piece.texture,
      )
    )
    
  private def draw(scene: Scene, pieces: Seq[PieceSprite], cfg: CanvasState) =
    
    val square = VecI(cfg.scale, cfg.scale)
    
    clearRect(VecI.zero, cfg.canvasSize)
    
    for (tile <- scene.board.labels) do
      val pos = gameToCanvasCornerCoords(tile.pos, cfg)
      fillRect(pos, square, tile.colour.hexString)
      
    for (piece <- pieces) do
      drawImage(piece.position, piece.size, piece.texture)
  
  private val updateStream: EventStream[(Scene, Seq[PieceSprite], CanvasState)] =
    Signal.combine(scene, pieces, config).changes
  
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
    Footer(),
  )