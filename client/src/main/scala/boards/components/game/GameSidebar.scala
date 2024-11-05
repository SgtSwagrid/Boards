package boards.components.game

import boards.components.game.GameBoard
import boards.graphics.Scene
import boards.graphics.Scene.{Input, PieceData, Tile}
import boards.imports.laminar.HtmlProp
import boards.protocol.GameProtocol.*
import boards.protocol.Room.Status
import boards.protocol.Room.Status.Complete
import boards.util.Navigation
import boards.views.GameView.{scene, socket}
import com.raquo.laminar.api.L.{div, img}
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
      child <-- scene.map { scene =>
        span (
          b(scene.room.game.name, fontSize("25px"), color("#2f3640")),
          b(s"#${scene.room.id}", fontSize("14px"), color("#84817a"), marginLeft("5px")),
        )
      }
    ),
    div (
      top("0"), left("0"), right("0"),
      height("50px"),
      paddingLeft("20px"), paddingTop("10px"), paddingRight("20px"),
      backgroundColor("#2f3640"),
      child <-- scene.map { scene =>
        scene.status match
          case Status.Pending =>
            p (
              textAlign("center"),
              className("text-warning"),
              "Waiting to Start",
            )
          case Status.Active =>
            p (
              textAlign("center"),
              b (
                scene.activePlayer.colour.textColour,
                if scene.isMyTurnAlone then "Your"
                else if scene.isExclusivelyHotseat then scene.activePlayer.name
                else scene.activePlayer.displayName,
              ),
              if scene.isMyTurnAlone then " Turn" else " to Play",
            )
          case Status.Complete =>
            scene.winner match
              case Some(winner) =>
                p (
                  textAlign("center"),
                  b (
                    winner.colour.textColour,
                    if scene.iWonAlone then "You"
                    else if scene.isExclusivelyHotseat then winner.name
                    else winner.displayName,
                  ),
                  " Won",
                )
              case None =>
                p (
                  textAlign("center"),
                  className("text-warning"),
                  "Draw",
                )
      },
    ),
    div (
      top("0"), left("0"), right("0"),
      padding("20px"),
      children <-- scene.map { scene =>
        
        scene.players.map { player =>
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
                span(
                  Colour.French.ParadiseGreen.textColour,
                  fontFamily("serif"),
                  player.suffix,
                ),
              ),
              p (
                span(player.colour.textColour, "⦿ "),
                fontSize("14px"),
                player.name,
                when (player.hasResigned) (
                  span (
                    className("text-error"),
                    " (Resigned)"
                  ),
                ),
              ),
            ),
            
            when (scene.isPending && scene.iAmPlaying) (
              div (
                className("tooltip"),
                display("inline-block"),
                marginTop("10px"),
                float("right"),
                dataTip := s"Remove ${player.username}",
                button (
                  className("btn btn-circle btn-ghost btn-sm"),
                  SVG.Cross,
                  onClick.mapTo(GameRequest.RemovePlayers(player.position)) --> response,
                ),
              ),
            ),
            
            when (scene.isActive && player.position == scene.activePlayerId) (
              img (
                display("inline-block"),
                float("right"),
                src("/assets/images/ui/game/active.svg"),
                width("30px"), height("30px"),
                verticalAlign("top"),
                marginTop("10px"),
              ),
            ),
            
            when (scene.isWinner(player.position)) (
              img (
                display("inline-block"),
                float("right"),
                src("/assets/images/ui/game/winner.svg"),
                width("30px"), height("30px"),
                verticalAlign("top"),
                marginTop("10px"),
              ),
            ),
          )
        }.interweave (
          scene.players.sliding(2).filter(_.size == 2).toSeq.map {
            case left :: right :: Nil =>
              div (
                className("divider"),
                marginTop(if scene.isPending && scene.iAmPlaying then "20px" else "10px"),
                marginBottom(if scene.isPending && scene.iAmPlaying then "20px" else "10px"),
                when (scene.isPending && scene.iAmPlaying && left.userId != right.userId) (
                  div (
                    className("tooltip"),
                    display("inline-block"),
                    float("right"),
                    dataTip := s"Swap ${left.username} and ${right.username}",
                    button (
                      className("btn btn-circle btn-ghost btn-sm"),
                      SVG.Swap,
                      onClick.mapTo(GameRequest.SwapPlayers(left.position, right.position)) --> response,
                    ),
                  ),
                ),
              )
          }
        )
      },
    ),
    div (
      bottom("0"), left("0"), right("0"),
      padding("20px"),
      position("absolute"),
      backgroundColor("#2f3640"),
      child <-- scene.map ( scene =>
        div (
          when (scene.isPending) (
            when (scene.iAmPlaying) (
              button (
                className("btn btn-error"),
                width("100%"),
                marginBottom("10px"),
                "Leave Game",
                onClick.mapTo(GameRequest.RemovePlayers(scene.myPlayers.map(_.position)*)) --> response,
              ),
            ),
            when (!scene.isFull) (
              button (
                className("btn btn-info"),
                width("100%"),
                marginBottom("10px"),
                if scene.iAmPlaying then "Add Hotseat Player" else "Join Game",
                if scene.iAmRegistered
                then onClick.mapTo(GameRequest.JoinRoom) --> response
                else onClick --> (_ => Navigation.goto("/login", "next" -> s"/game/${scene.room.id}/join"))
              ),
            ),
            when (scene.canStart && scene.iAmPlaying) (
              button (
                className("btn btn-accent"),
                width("100%"),
                margin("0 0 10px 0"),
                "Start Game",
                onClick.mapTo(GameRequest.StartGame) --> response,
              ),
            ),
          ),
          when (scene.isActive && scene.iAmPlaying) (
            div (
              /*button (
                className("btn btn-warning"),
                width("100%"),
                marginBottom("10px"),
                "Offer Draw",
                onClick.mapTo(GameRequest.OfferDraw(???)) --> response,
              ),*/
              {
                val (toRejoin, toResign) =
                  (if scene.isMyTurn then Seq(scene.activePlayer) else scene.myPlayers)
                    .partition(_.hasResigned)
                
                if toResign.nonEmpty then
                  button (
                    className("btn btn-error"),
                    width("100%"),
                    marginBottom("10px"),
                    if scene.iAmPlayingAlone then "Resign Game"
                    else if toResign.sizeIs == 1 then s"Resign as ${toResign.head.name}"
                    else "Resign All",
                    onClick.mapTo(GameRequest.Resign(true, toResign.map(_.position)*)) --> response,
                  )
                else if toRejoin.nonEmpty then
                  button (
                    className("btn btn-info"),
                    width("100%"),
                    marginBottom("10px"),
                    if scene.iAmPlayingAlone then "Rejoin Game"
                    else if toRejoin.sizeIs == 1 then s"Rejoin as ${toRejoin.head.name}"
                    else "Rejoin All",
                    onClick.mapTo(GameRequest.Resign(false, toRejoin.map(_.position)*)) --> response,
                  )
                else emptyNode
              },
            ),
          ),
        ),
      ),
    ),
  )