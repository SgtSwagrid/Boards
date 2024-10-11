package boards.components.game

import boards.components.game.GameBoard
import boards.graphics.Scene
import boards.graphics.Scene.{Input, PieceData, Tile}
import boards.imports.laminar.HtmlProp
import boards.protocol.GameProtocol.{GameRequest, Player, Spectator, Status, Unregistered}
import boards.views.GameView.{scene, socket}
import com.raquo.laminar.api.L.img
import com.raquo.laminar.codecs.StringAsIsCodec
import com.raquo.laminar.modifiers.RenderableText

import scala.scalajs.js.annotation.JSExportTopLevel
//import boards.imports.laminar.{*, given}
import boards.imports.circe.{*, given}
import boards.imports.math.{*, given}
import boards.imports.games.{*, given}
import boards.util.extensions.ColourOps.{*, given}
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

object GameSidebar:
  val sidebarWidth: Int = 250

class GameSidebar(scene: Signal[Scene], response: Observer[GameRequest]):
  
  private val dataTip: HtmlAttr[String] =
    htmlAttr("data-tip", StringAsIsCodec)
  
  def apply: HtmlElement = div (
    position("absolute"),
    top(s"${Navbar.navbarHeight}px"),
    bottom(s"${Footer.footerHeight}px"),
    left("0px"),
    width(s"${GameSidebar.sidebarWidth}px"),
    backgroundColor("#353b48"),
    div (
      top("0"), left("0"), right("0"),
      height("60px"),
      paddingLeft("20px"), paddingTop("10px"),
      //className("bg-primary-content"),
      backgroundColor("#fbc531"),
      child <-- scene.map: scene =>
        span (
          b(scene.room.game.name, fontSize("25px"), color("#2f3640")),
          b(s"#${scene.room.id}", fontSize("14px"), color("#84817a"), marginLeft("5px")),
        )
    ),
    div (
      top("0"), left("0"), right("0"),
      height("50px"),
      paddingLeft("20px"), paddingTop("10px"), paddingRight("20px"),
      backgroundColor("#2f3640"),
      child <-- scene.map: scene =>
        scene.room.status match
          case Status.Pending => p("Waiting to start", className("text-warning"), textAlign("center"))
          case Status.Active => p (
            textAlign("center"),
            b (
              scene.activePlayerColour.textColour,
              scene.activePlayerName,
            ),
            " to play",
          )
          case Status.Complete => p("Game ended", className("text-info"), textAlign("center"))
    ),
    div (
      top("0"), left("0"), right("0"),
      padding("20px"),
      children <-- scene.map: scene =>
        
        scene.players.map: player =>
          span (
            display("block"),
            width("100%"),
            img (
              display("inline-block"),
              marginLeft("10px"),
              src("/assets/images/ui/game/player.svg"),
              width("30px"), height("30px"),
              verticalAlign("top"),
              marginTop("10px"),
            ),
            div (
              display("inline-block"),
              marginLeft("15px"),
              b (
                fontSize("16px"),
                player.username,
              ),
              p (
                span(scene.game.playerColours(player.position).textColour, "⦿ "),
                fontSize("14px"),
                scene.game.playerNames(player.position),
              )
            ),
            
            Option.when(scene.status.isPending && scene.participant.isPlayer):
              div (
                className("tooltip"),
                display("inline-block"),
                marginTop("10px"),
                float("right"),
                dataTip := s"Remove ${player.username}",
                button (
                  className("btn btn-circle btn-outline btn-sm btn-error"),
                  SVG.Cross,
                  onClick.mapTo(GameRequest.RemovePlayer(player.userId)) --> response,
                ),
              ),
            
            Option.when(scene.status.isActive && player.position == scene.activePlayerId):
              img (
                display("inline-block"),
                float("right"),
                src("/assets/images/ui/game/active.svg"),
                width("30px"), height("30px"),
                verticalAlign("top"),
                marginTop("10px"),
              )
          )
        .interweave:
          scene.players.sliding(2).filter(_.size == 2).toSeq.map:
            case player1 :: player2 :: Nil =>
              val canSetup = scene.participant.isPlayer && scene.status.isPending
              div (
                className("divider"),
                marginTop(if canSetup then "20px" else "10px"),
                marginBottom(if canSetup then "20px" else "10px"),
                Option.when(canSetup):
                  div (
                    className("tooltip"),
                    display("inline-block"),
                    float("right"),
                    dataTip := s"Swap ${player1.username} and ${player2.username}",
                    button (
                      className("btn btn-circle btn-outline btn-sm btn-warning"),
                      SVG.Swap,
                      onClick.mapTo(GameRequest.ReorderPlayer(player2.userId)) --> response,
                    )
                  ),
              )
    ),
    div (
      bottom("0"), left("0"), right("0"),
      padding("20px"),
      position("absolute"),
      backgroundColor("#2f3640"),
      child.maybe <-- scene.map: scene =>
        Option.when(scene.status.isPending):
          scene.participant match
            case player: Player =>
              div (
                button (
                  className("btn btn-error"),
                  width("100%"),
                  marginBottom("10px"),
                  "Leave Game",
                  onClick.mapTo(GameRequest.RemovePlayer(player.userId)) --> response,
                ),
                Option.when(scene.game.numPlayers.contains(scene.players.size)):
                  button (
                    className("btn btn-accent"),
                    width("100%"),
                    margin("0 0 10px 0"),
                    "Start Game",
                    onClick.mapTo(GameRequest.StartGame) --> response,
                  )
              )
            case Spectator(_, _) =>
              button (
                className("btn btn-info"),
                width("100%"),
                margin("0 0 10px 0"),
                "Join Game",
                onClick.mapTo(GameRequest.JoinRoom) --> response,
              )
            case Unregistered => div()
    )
  )