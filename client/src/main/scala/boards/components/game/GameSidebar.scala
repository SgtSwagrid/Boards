package boards.components.game

import boards.components.{Footer, Navbar, SVG}
import boards.graphics.{Colour, Scene}
import boards.protocol.GameProtocol.GameRequest
import boards.protocol.Room.{RichPlayer, Status}
import boards.util.Navigation.goto
import boards.util.extensions.SequenceOps.interweave
import boards.util.extensions.ColourOps.textColour
import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.codecs.StringAsIsCodec


object GameSidebar:
  val sidebarWidth: Int = 250

/**
 * The panel which appears on the left-hand side while playing a game.
 * @param scene The current scene to display as provided by the server.
 * @param respond An observer for providing responses to the server.
 */
class GameSidebar(scene: Scene, respond: Observer[GameRequest]):
  
  private val dataTip: HtmlAttr[String] =
    htmlAttr("data-tip", StringAsIsCodec)
  
  def apply: HtmlElement =
    
    div (
      position("absolute"),
      top(s"${Navbar.navbarHeight}px"),
      bottom(s"${Footer.footerHeight}px"),
      left("0px"),
      width(s"${GameSidebar.sidebarWidth}px"),
      backgroundColor("#353b48"),
      headerPanel,
      statusPanel,
      playerPanel,
      buttonPanel,
    )
  
  private def headerPanel =
    
    div (
      top("0"), left("0"), right("0"),
      height("60px"),
      paddingLeft("20px"), paddingTop("10px"),
      //className("bg-primary-content"),
      backgroundColor("#fbc531"),
      span (
        b(scene.room.game.name, fontSize("25px"), color("#2f3640")),
        b(s"#${scene.room.id}", fontSize("14px"), color("#84817a"), marginLeft("5px")),
      ),
    )
  
  private def statusPanel =
    
    div (
      top("0"), left("0"), right("0"),
      height("50px"),
      paddingLeft("20px"), paddingTop("10px"), paddingRight("20px"),
      backgroundColor("#2f3640"),
      if scene.status.isPending then pendingStatus
      else if scene.outcome.isDefined then completeStatus
      else activeStatus
    )
    
  private def pendingStatus =
    
    p (
      textAlign("center"),
      className("text-warning"),
      "Waiting to Start",
    )
    
  private def activeStatus =
    
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
    
  private def completeStatus =
    
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
  
  private def playerPanel =
    
    div (
      top("0"), left("0"), right("0"),
      padding("20px"),
      scene.players.map(playerInfo).interweave:
        scene.players.sliding(2).filter(_.size == 2).toSeq.map:
          case Seq(left, right) => playerDivider(left, right)
    )
  
  private def playerInfo(player: RichPlayer) =
    
    span (
      display("block"),
      width("100%"),
      playerIcon,
      playerName(player),
      removePlayerButton(player),
      playerTurnMarker(player),
      playerWinMarker(player),
    )
  
  private def playerIcon =
    
    img (
      display("inline-block"),
      marginLeft("10px"),
      src("/assets/images/ui/game/player.svg"),
      width("30px"), height("30px"),
      verticalAlign("top"),
      marginTop("10px"),
    )
  
  private def playerName(player: RichPlayer) =
    
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
      if player.hasResigned then
        p (
          className("text-error"),
          fontSize("12px"),
          "(Resigned)",
        )
      else if player.hasOfferedDraw then
        p (
          className("text-warning"),
          fontSize("12px"),
          "(Offered Draw)",
        )
      else emptyNode,
    )
  
  private def removePlayerButton(player: RichPlayer) =
    
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
  
  private def playerTurnMarker(player: RichPlayer) =
    
    when (!scene.isPending && scene.outcome.isEmpty && player.position == scene.activePlayerId) (
      img (
        display("inline-block"),
        float("right"),
        src("/assets/images/ui/game/active.svg"),
        width("30px"), height("30px"),
        verticalAlign("top"),
        marginTop("10px"),
      ),
    )
  
  private def playerWinMarker(player: RichPlayer) =
    
    when (scene.isWinner(player.position)) (
      img (
        display("inline-block"),
        float("right"),
        src("/assets/images/ui/game/winner.svg"),
        width("30px"), height("30px"),
        verticalAlign("top"),
        marginTop("10px"),
      ),
    )
  
  private def playerDivider(left: RichPlayer, right: RichPlayer) =
    
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
  
  private def buttonPanel =
    
    div (
      bottom("0"), left("0"), right("0"),
      padding("20px"),
      position("absolute"),
      backgroundColor("#2f3640"),
      if scene.isPending then div (
        leaveButton,
        joinButton,
        startButton,
      ) else if scene.isActive && scene.iAmPlaying then div (
        drawButton,
        resignButton,
      ) else if scene.isComplete then
        navigationButtons
      else emptyNode,
    )
  
  private def leaveButton =
    
    when (scene.iAmPlaying) (
      button (
        className("btn btn-error"),
        width("100%"),
        marginBottom("10px"),
        "Leave Game",
        onClick.mapTo(GameRequest.RemovePlayers(scene.myPlayers.map(_.position)*)) --> respond,
      )
    )
  
  private def joinButton =
    
    when (!scene.isFull) (
      button (
        className("btn btn-info"),
        width("100%"),
        marginBottom("10px"),
        if scene.iAmPlaying then "Add Hotseat Player" else "Join Game",
        if scene.iAmRegistered
        then onClick.mapTo(GameRequest.JoinRoom) --> respond
        else onClick --> (_ => goto("/login", "next" -> s"/game/${scene.room.id}/join"))
      ),
    )
  
  private def startButton =
    
    when (scene.canStart && scene.iAmPlaying) (
      button (
        className("btn btn-accent"),
        width("100%"),
        margin("0 0 10px 0"),
        "Start Game",
        onClick.mapTo(GameRequest.StartGame) --> respond,
      ),
    )
  
  private def drawButton =
    
    if scene.iHaveResignedAll then emptyNode else
      if !scene.iHaveOfferedDraw then
        button (
          className("btn btn-warning"),
          width("100%"),
          marginBottom("10px"),
          if scene.isExclusivelyHotseat then "Declare Draw"
          else if scene.someoneHasOfferedDraw then "Accept Draw"
          else "Offer Draw",
          onClick.mapTo(GameRequest.OfferDraw(true, scene.myActivePlayers.map(_.position)*)) --> respond,
        )
      else
        button (
          className("btn btn-warning"),
          width("100%"),
          marginBottom("10px"),
          "Revoke Draw",
          onClick.mapTo(GameRequest.OfferDraw(false, scene.myDrawnPlayers.map(_.position)*)) --> respond,
        )
  
  private def resignButton =
    
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
        onClick.mapTo(GameRequest.Resign(true, toResign.map(_.position) *)) --> respond,
      )
    else if toRejoin.nonEmpty then
      button (
        className("btn btn-info"),
        width("100%"),
        marginBottom("10px"),
        if scene.iAmPlayingAlone then "Rejoin Game"
        else if toRejoin.sizeIs == 1 then s"Rejoin as ${toRejoin.head.name}"
        else "Rejoin All",
        onClick.mapTo(GameRequest.Resign(false, toRejoin.map(_.position) *)) --> respond,
      )
    else emptyNode
    
  private def navigationButtons =
    
    div (
      margin("auto"),
      maxWidth("fit-content"),
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
          onClick.mapTo(GameRequest.ViewPreviousState(0)) --> respond,
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
          onClick.mapTo(GameRequest.ViewPreviousState(scene.time - 1)) --> respond,
        ),
      ),
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
          onClick.mapTo(GameRequest.ViewPreviousState(scene.time + 1)) --> respond,
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
          onClick.mapTo(GameRequest.ViewPreviousState(-1)) --> respond,
        ),
      ),
    )
    