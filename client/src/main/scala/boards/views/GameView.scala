package boards.views

import boards.components.game.{GameBoard, GameSidebar}
import boards.graphics.Scene
import boards.graphics.Scene.{PieceData, Tile}
import boards.imports.laminar.HtmlProp
import boards.protocol.GameProtocol.*
import com.raquo.laminar.codecs.StringAsIsCodec
import com.raquo.laminar.modifiers.RenderableText

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

@JSExportTopLevel("GameView")
object GameView extends View:
  
  private val roomId: String =
    document.documentURI.split("/").dropWhile(_ != "game").drop(1).head
    
  private val autoJoin: Boolean =
    document.documentURI.split("/").contains("join")
  
  private val socket: WebSocket[Scene, GameRequest] =
    WebSocket.path(s"/game/$roomId/socket").json[Scene, GameRequest].build()
    
  private val sceneBus: EventBus[Scene] = new EventBus[Scene]
  private val scene: Signal[Scene] = sceneBus.events.startWith(Scene.empty)
  
  val boardPadding = 50
  
  def content = div (
    socket.connect,
    socket.connected.filter(_ => autoJoin).mapTo(GameRequest.JoinRoom) --> socket.send,
    socket.received --> sceneBus.writer,
    //socket.received --> {x => println(x.choices)},
    
    Navbar(),
    
    child <-- scene.map(scene => GameSidebar(scene, socket.send).apply),
    
    div (
      position("absolute"),
      top(s"${Navbar.navbarHeight + boardPadding}px"),
      bottom(s"${Footer.footerHeight + boardPadding}px"),
      left(s"${GameSidebar.sidebarWidth + boardPadding}px"),
      right(s"${boardPadding}px"),
      GameBoard(sceneBus, socket.send).apply,
    ),
    Footer(),
  )