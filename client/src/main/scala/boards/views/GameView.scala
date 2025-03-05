package boards.views

import boards.components.game.{GameBoard, GameSidebar}
import boards.graphics.Scene
import boards.graphics.Scene.PieceData
import boards.math.vector.Embedding.Tile
import boards.protocol.GameProtocol.*
import boards.protocol.UserProtocol.User
import boards.util.Navigation.*
import com.raquo.laminar.codecs.StringAsIsCodec
import com.raquo.laminar.modifiers.RenderableText
import org.scalajs.dom.Audio

import scala.concurrent.duration.*
import scala.scalajs.js.annotation.JSExportTopLevel
import com.raquo.laminar.nodes.ChildNode
import org.scalajs.dom.{CanvasRenderingContext2D, DOMRect, HTMLImageElement, MouseEvent}
import com.raquo.laminar.api.features.unitArrows

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
import io.circe.generic.auto.*
import boards.util.Codecs.{*, given}
import boards.components.{ExpandingButton, Footer, InputField, Navbar, SVG, Tabs}
import boards.dsl.meta.TurnId
import boards.dsl.meta.TurnId.TurnId

@JSExportTopLevel("GameView")
object GameView extends View:
  
  private val query = document.documentURI.split("/").dropWhile(_ != "game").drop(1)
  private lazy val roomId: String = query.head.split(":")(0)
  private lazy val turnId: Option[TurnId] = query.head.split(":").lift(1).map(_.toInt).map(TurnId.apply)
  
  private val autoJoin: Boolean = document.documentURI.split("/").contains("join")
  private val autoFork: Boolean = document.documentURI.split("/").contains("fork")
  
  private lazy val socket: WebSocket[GameResponse, GameRequest] = WebSocket
    .path(s"/game/$roomId/socket")
    .json[GameResponse, GameRequest]
    .build(autoReconnect = true, reconnectDelay = 2.seconds, reconnectRetries = 20)
    
  private val sceneBus: EventBus[Scene] = new EventBus[Scene]
  private val scene: Signal[Scene] = sceneBus.events.startWith(Scene.empty)
  
  private val starts: EventStream[?] = scene.map(_.isPending).changes.distinct.filter(x => !x)
  private val updates: EventStream[?] = scene.map(_.pieces).changes.distinct
  private val wins: EventStream[?] = scene.map(_.iWon).changes.distinct.filter(x => x)
  private val losses: EventStream[?] = scene.map(_.iLost).changes.distinct.filter(x => x)
  private val draws: EventStream[?] = scene.map(_.isDraw).changes.distinct.filter(x => x)
  
  private val startSound = Audio("/assets/audio/start.mp3")
  private val placeSound = Audio("/assets/audio/place.mp3")
  private val winSound = Audio("/assets/audio/win.mp3")
  private val loseSound = Audio("/assets/audio/lose.mp3")
  private val drawSound = Audio("/assets/audio/draw.mp3")
  
  private val sounds: EventStream[Audio] = EventStream.merge (
    starts.mapTo(startSound),
    updates.mapTo(placeSound),
    wins.mapTo(winSound),
    losses.mapTo(loseSound),
    draws.mapTo(drawSound),
  )
  
  val boardPadding = 30
  
  def content (user: Option[User]) = div (
    socket.connect,
    socket.connected.filter(_ => autoJoin).mapTo(GameRequest.JoinRoom(1)) --> socket.send,
    socket.connected.filter(_ => autoFork).mapTo(GameRequest.ForkState(turnId)) --> socket.send,
    socket.connected.mapTo(turnId).filter(_.isDefined).map(_.get)
      .map(GameRequest.ViewTurnId.apply) --> socket.send,
    socket.received.collect { case GameResponse.Render(scene) => scene } --> sceneBus.writer,
    socket.received.collect { case GameResponse.Goto(id) => id } --> (id => goto(s"/game/$id")),
    
    Navbar(user),
    
    GameSidebar(scene, socket.send).apply,
    
    div (
      position("absolute"),
      top(s"${Navbar.navbarHeight + boardPadding}px"),
      bottom(s"${Footer.footerHeight + boardPadding}px"),
      left(s"${GameSidebar.sidebarWidth + boardPadding}px"),
      right(s"${boardPadding}px"),
      GameBoard(sceneBus, socket.send).apply,
      sounds --> { _.play() },
    ),
    Footer(),
  )