package boards.components.game

import boards.components.{BlockButton, Footer, Navbar, SVG}
import boards.dsl.meta.Game.Property
import boards.dsl.meta.TurnId
import boards.graphics.{Colour, Scene}
import boards.protocol.GameProtocol.GameRequest
import boards.protocol.Room.{RichPlayer, Status}
import boards.util.Tags.*
import boards.util.Navigation.goto
import boards.util.extensions.SequenceOps.interweave
import boards.util.extensions.ColourOps.textColour
import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.codecs.StringAsIsCodec
import com.raquo.laminar.keys.HtmlProp
import com.raquo.laminar.api.features.unitArrows
import org.scalajs.dom.window.navigator

object GameSidebar:
  val sidebarWidth: Int = 250

/**
 * The panel which appears on the left-hand side while playing a game.
 * @param scene The current scene to display as provided by the server.
 * @param respond An observer for providing responses to the server.
 */
class GameSidebar (scene: Signal[Scene], respond: Observer[GameRequest]):
  
  def apply: HtmlElement =
    
    div (
      position("absolute"),
      top(s"${Navbar.navbarHeight}px"),
      bottom(s"${Footer.footerHeight}px"),
      left("0px"),
      width(s"${GameSidebar.sidebarWidth}px"),
      backgroundColor("#353b48"),
      div (
        backgroundColor("#353b48"),
        zIndex("2"),
        headerPanel,
        child <-- scene.map(implicit scene => statusPanel),
        child <-- scene.map(implicit scene => playerPanel),
      ),
      div (
        bottom("0"), left("0"), right("0"),
        position("absolute"),
        backgroundColor("#353b48"),
        configPanel,
        child <-- scene.map(implicit scene => buttonPanel),
      ),
    )
  
  private def headerPanel =
    
    val showCopyMessage = new EventBus[Boolean]
    
    div (
      top("0"), left("0"), right("0"),
      paddingLeft("20px"),
      paddingTop <-- scene.map: scene =>
        if scene.forkedFrom.isDefined || scene.rematchOf.isDefined then "5px" else "10px"
      ,
      height("60px"),
      zIndex("4"),
      backgroundColor("#fbc531"),
      children <-- scene.map { scene => Seq (
        span (
          b (
            scene.room.game.name,
            fontSize("25px"),
            color("#2f3640"),
          ),
          span (
            marginLeft("5px"),
            cursor("pointer"),
            className <-- showCopyMessage.stream.startWith(false).map:
              case true => "tooltip tooltip-open tooltip-bottom tooltip-primary"
              case false => ""
            ,
            dataTip("Copied!"),
            b (
              s"#${scene.room.id}",
              fontSize("14px"),
              color("#74716a"),
              onClick --> copyToClipboard(scene.room.id),
              onClick.mapTo(true) --> showCopyMessage.writer,
              onClick.mapTo(false) --> showCopyMessage.writer.delay(1000),
            ),
          ),
        ),
        scene.forkedFrom.map(_.id) zip scene.forkedTurn match {
          case Some((forkedFrom, forkedTurn)) =>
            p (
              fontSize("12px"),
              color("#74716a"),
              marginTop("-5px"),
              "Forked from ",
              a (
                s"#$forkedFrom:$forkedTurn",
                color(Colour.British.MattPurple.hexString),
                href(s"/game/$forkedFrom:$forkedTurn"),
              ),
            )
          case None => emptyNode
        },
        scene.rematchOf.map(_.id) match {
          case Some(rematchOf) =>
            p (
              fontSize("12px"),
              color("#74716a"),
              marginTop("-5px"),
              "Rematch of ",
              a (
                s"#$rematchOf",
                color(Colour.British.MattPurple.hexString),
                href(s"/game/$rematchOf"),
              ),
            )
          case None => emptyNode
        }
      )}
    )
  
  private def statusPanel =
    
    div (
      top("0"), left("0"), right("0"),
      height("50px"),
      paddingLeft("20px"), paddingTop("10px"), paddingRight("20px"),
      backgroundColor("#2f3640"),
      zIndex("4"),
      child <-- scene.map { implicit scene =>
        if scene.isPending then pendingStatus
        else if scene.isActiveHere then activeStatus
        else completeStatus
      }
    )
    
  private def pendingStatus =
    
    p (
      textAlign("center"),
      className("text-warning"),
      "Waiting to Start",
    )
    
  private def activeStatus(using scene: Scene) =
    
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
    
  private def completeStatus(using scene: Scene) =
    
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
  
  private def playerPanel(using scene: Scene) =
    
    div (
      top("0"), left("0"), right("0"),
      padding("20px"),
      scene.players.map(playerInfo).interweave:
        scene.players.sliding(2).filter(_.size == 2).toSeq.map:
          case Seq(left, right) => playerDivider(left, right)
    )
  
  private def playerInfo(player: RichPlayer)(using scene: Scene) =
    
    span (
      display("block"),
      width("100%"),
      playerIcon,
      playerName(player),
      removePlayerButton(player),
      playerTurnMarker(player),
      playerWinMarker(player),
    )
  
  private def playerIcon(using scene: Scene) =
    
    img (
      display("inline-block"),
      marginLeft("10px"),
      src("/assets/images/ui/game/player.svg"),
      width("30px"), height("30px"),
      verticalAlign("top"),
      marginTop("10px"),
    )
  
  private def playerName(player: RichPlayer)(using scene: Scene) =
    
    div (
      display("inline-block"),
      marginLeft("15px"),
      b (
        fontSize("16px"),
        player.username,
        span (
          Colour.British.ChainGangGrey.textColour,
          fontFamily("serif"),
          player.suffix,
        ),
      ),
      p (
        player.colour.textColour,
        fontSize("14px"),
        player.name,
      ),
      if scene.isLatestState && player.hasResigned then
        p (
          className("text-error"),
          fontSize("12px"),
          "(Resigned)",
        )
      else if scene.isLatestState && player.hasOfferedDraw then
        p (
          className("text-warning"),
          fontSize("12px"),
          "(Offered Draw)",
        )
      else emptyNode,
    )
  
  private def removePlayerButton(player: RichPlayer)(using scene: Scene) =
    
    when (scene.isPending && scene.iAmPlaying) (
      div (
        className("tooltip"),
        display("inline-block"),
        marginTop("10px"),
        float("right"),
        dataTip(s"Remove ${player.username}"),
        button (
          className("btn btn-circle btn-ghost btn-sm"),
          SVG.Cross,
          onClick.mapTo(GameRequest.RemovePlayers(player.position)) --> respond,
        ),
      ),
    )
  
  private def playerTurnMarker(player: RichPlayer)(using scene: Scene) =
    
    when (scene.isActiveHere && player.position == scene.activePlayerId) (
      img (
        display("inline-block"),
        float("right"),
        src("/assets/images/ui/game/active.svg"),
        width("30px"), height("30px"),
        verticalAlign("top"),
        marginTop("10px"),
      ),
    )
  
  private def playerWinMarker(player: RichPlayer)(using scene: Scene) =
    
    when (scene.isLatestState && scene.isWinner(player.position)) (
      img (
        display("inline-block"),
        float("right"),
        src("/assets/images/ui/game/winner.svg"),
        width("30px"), height("30px"),
        verticalAlign("top"),
        marginTop("10px"),
      ),
    )
  
  private def playerDivider(left: RichPlayer, right: RichPlayer)(using scene: Scene) =
    
    val canSwap = scene.isPending && scene.iAmPlaying
    
    div (
      className("divider"),
      marginTop(if canSwap then "20px" else "10px"),
      marginBottom(if canSwap then "20px" else "10px"),
      when(canSwap && left.userId != right.userId) (
        div (
          className("tooltip"),
          display("inline-block"),
          float("right"),
          dataTip(s"Swap ${left.username} and ${right.username}"),
          button (
            className("btn btn-circle btn-ghost btn-sm"),
            SVG.Swap,
            onClick.mapTo(GameRequest.SwapPlayers(left.position, right.position)) --> respond,
          ),
        ),
      ),
    )
    
  private def configPanel =
    
    val showSliders = scene.map: scene =>
      scene.isPending && scene.iAmPlaying && scene.forkedFrom.isEmpty
    .distinct
    
    div (
      position("relative"),
      bottom("0px"),
      child <-- showSliders.map: showSliders =>
        div (
          padding("20px"),
          children <-- scene.map(scene => scene.game.properties.map(prop => (prop, scene)))
            .split(_(0).name) { case (_, (property, initialScene), _) =>
              propertySlider(property, initialScene.property(property.name), showSliders)
            }
        )
    )
    
  private def propertySlider(property: Property, default: Int, showSlider: Boolean = false) =
    
    val updates = new EventBus[Int]
    
    div (
      div(className("divider"), marginTop("0px"), marginBottom("5px")),
      scene.map(_.property(property.name)).changes.distinct --> updates.writer,
      padding("5px"),
      span (
        fontSize("16px"),
        b (
          property.name,
        ),
        b (
          display("inline-block"),
          float("right"),
          color(Colour.British.RiseNShine.hexString),
          text <-- updates.stream.startWith(default),
        ),
      ),
      when (showSlider) (
        input (
          marginTop("5px"),
          `type`("range"),
          className("range"),
          min(property.min.toString),
          max(property.max.toString),
          value <-- updates.stream.startWith(default).map(_.toString),
          step("1"),
          onInput.mapToValue.map(i => GameRequest.SetProperty(property.name, i.toInt)) --> respond,
          onInput.mapToValue.map(_.toInt) --> updates.writer,
        ),
      ),
    )
  
  private def buttonPanel(using scene: Scene) =
    
    div (
      padding("20px"),
      backgroundColor("#2f3640"),
      zIndex("3"),
      if scene.isPending then div (
        leaveButton,
        joinButton,
        startButton,
      ) else if scene.isActive && scene.iAmPlaying then div (
        drawButton,
        resignButton,
      ) else if scene.isComplete then div (
        rematchButton,
        forkButton,
        navigationButtons,
      ) else emptyNode,
    )
  
  private def leaveButton(using scene: Scene) =
    
    when (scene.iAmPlaying) (
      BlockButton("error") (
        "Leave Game",
        onClick.mapTo(GameRequest.RemovePlayers(scene.myPlayers.map(_.position)*)) --> respond,
      )
    )
  
  private def joinButton(using scene: Scene) =
    
    when (!scene.isFull) (
      BlockButton("info") (
        if scene.iAmPlaying then "Add Hotseat Player" else "Join Game",
        if scene.iAmRegistered
        then onClick.mapTo(GameRequest.JoinRoom(1)) --> respond
        else onClick --> goto("/login", "next" -> s"/game/${scene.room.id}/join"),
      ),
    )
  
  private def startButton(using scene: Scene) =
    
    when (scene.canStart && scene.iAmPlaying) (
      BlockButton("accent") (
        "Start Game",
        onClick.mapTo(GameRequest.StartGame) --> respond,
      ),
    )
  
  private def drawButton(using scene: Scene) =
    
    if scene.iHaveResignedAll then emptyNode else
      if !scene.iHaveOfferedDraw then
        BlockButton("warning") (
          if scene.isExclusivelyHotseat then "Declare Draw"
          else if scene.someoneHasOfferedDraw then "Accept Draw"
          else "Offer Draw",
          onClick.mapTo(GameRequest.OfferDraw(true, scene.myActivePlayers.map(_.position)*)) --> respond,
        )
      else
        BlockButton("warning") (
          "Revoke Draw",
          onClick.mapTo(GameRequest.OfferDraw(false, scene.myDrawnPlayers.map(_.position)*)) --> respond,
        )
  
  private def resignButton(using scene: Scene) =
    
    val (toRejoin, toResign) =
      (if scene.isMyTurn then Seq(scene.activePlayer) else scene.myPlayers)
        .partition(_.hasResigned)
    
    if toResign.nonEmpty then
      BlockButton("error") (
        if scene.iAmPlayingAlone then "Resign Game"
        else if toResign.sizeIs == 1 then s"Resign as ${toResign.head.name}"
        else "Resign All",
        onClick.mapTo(GameRequest.Resign(true, toResign.map(_.position)*)) --> respond,
      )
    else if toRejoin.nonEmpty then
      BlockButton("info") (
        if scene.iAmPlayingAlone then "Rejoin Game"
        else if toRejoin.sizeIs == 1 then s"Rejoin as ${toRejoin.head.name}"
        else "Rejoin All",
        onClick.mapTo(GameRequest.Resign(false, toRejoin.map(_.position)*)) --> respond,
      )
    else emptyNode
    
  private def rematchButton(using scene: Scene) =
    
    if scene.iCanOfferRematch || scene.iCanJoinRematch then BlockButton("warning") (
      if scene.rematch.isEmpty
      then if scene.iAmPlayingExclusivelyHotseat then "Play Rematch" else "Offer Rematch"
      else "Accept Rematch",
      onClick.mapTo(GameRequest.OfferRematch) --> respond,
    ) else if scene.rematch.nonEmpty then BlockButton("warning") (
      "View Rematch",
      onClick --> goto(s"/game/${scene.rematch.get.id}"),
    ) else emptyNode
    
  private def forkButton(using scene: Scene) =
    
    if scene.logicalOutcome.isDefined && !scene.isActiveHere then emptyNode else
      BlockButton("info") (
        "Fork From Here",
        if scene.iAmRegistered
        then onClick.mapTo(GameRequest.ForkState(Some(scene.currentTurnId))) --> respond
        else onClick --> goto("/login", "next" -> s"/game/${scene.room.id}:${scene.currentTurnId}/fork"),
      )
    
  /** The forward and back buttons which appear when the game is over to browse past states. */
  private def navigationButtons(using scene: Scene) =
    
    div (
      windowEvents(_.onWheel).filter(_ => scene.isComplete).collect {
        case e if e.deltaY > 0 && !scene.isLatestState => GameRequest.ViewTurnId(scene.currentTurnId.next)
        case e if e.deltaY < 0 && !scene.isInitialState => GameRequest.ViewTurnId(scene.currentTurnId.previous)
      } --> respond,
      
      margin("auto"),
      maxWidth("fit-content"),
      when (!scene.isInitialState) (
        div (
          margin("5px"),
          display("inline-block"),
          className("tooltip"),
          dataTip("To Start of Game"),
          button (
            className("btn btn-square btn-sm btn-ghost"),
            img (
              src("/assets/images/ui/game/first.svg"),
              width("30px"), height("30px"),
            ),
            onClick.mapTo(GameRequest.ViewTurnId(TurnId.initial)) --> respond,
          ),
        ),
        div (
          margin("5px"),
          display("inline-block"),
          className("tooltip"),
          dataTip("To Previous Turn"),
          button (
            className("btn btn-square btn-sm btn-ghost"),
            img (
              src("/assets/images/ui/game/previous.svg"),
              width("30px"), height("30px"),
            ),
            onClick.mapTo(GameRequest.ViewTurnId(scene.currentTurnId.previous)) --> respond,
          ),
        ),
      ),
      when (scene.isInitialState) (
        div (
          margin("5px"),
          display("inline-block"),
          button (
            className("btn btn-square btn-sm btn-ghost btn-disabled"),
            img (
              src("/assets/images/ui/game/first_disabled.svg"),
              width("30px"), height("30px"),
            ),
          ),
        ),
        div (
          margin("5px"),
          display("inline-block"),
          button (
            className("btn btn-square btn-sm btn-ghost btn-disabled"),
            img (
              src("/assets/images/ui/game/previous_disabled.svg"),
              width("30px"), height("30px"),
            ),
          ),
        ),
      ),
      when (!scene.isLatestState) (
        div (
          margin("5px"),
          display("inline-block"),
          className("tooltip"),
          dataTip("To Next Turn"),
          button (
            className("btn btn-square btn-sm btn-ghost"),
            img (
              src("/assets/images/ui/game/next.svg"),
              width("30px"), height("30px"),
            ),
            onClick.mapTo(GameRequest.ViewTurnId(scene.currentTurnId.next)) --> respond,
          ),
        ),
        div (
          margin("5px"),
          display("inline-block"),
          className("tooltip"),
          dataTip("To End of Game"),
          button (
            className("btn btn-square btn-sm btn-ghost"),
            img (
              src("/assets/images/ui/game/last.svg"),
              width("30px"), height("30px"),
            ),
            onClick.mapTo(GameRequest.ViewTurnId(scene.latestTurnId)) --> respond,
          ),
        ),
      ),
      when (scene.isLatestState) (
        div (
          margin("5px"),
          display("inline-block"),
          button (
            className("btn btn-square btn-sm btn-ghost btn-disabled"),
            img (
              src("/assets/images/ui/game/next_disabled.svg"),
              width("30px"), height("30px"),
            ),
          ),
        ),
        div (
          margin("5px"),
          display("inline-block"),
          button (
            className("btn btn-square btn-sm btn-ghost btn-disabled"),
            img (
              src("/assets/images/ui/game/last_disabled.svg"),
              width("30px"), height("30px"),
            ),
          ),
        ),
      ),
    )
    
  private def copyToClipboard(text: String): Unit =
    navigator.clipboard.writeText(text)
  
end GameSidebar