package controllers

import RoomActor.Subscriber
import RoomActor.Protocol.*
import boards.bots.BotCatalogue
import boards.dsl.meta.TurnId
import boards.dsl.meta.TurnId.{next, TurnId}
import boards.dsl.rules.Cause
import boards.dsl.states.GameState
import boards.dsl.states.InstantaneousState.given
import boards.graphics.Scene
import boards.protocol.GameProtocol.*
import boards.protocol.GameProtocol.GameRequest.*
import boards.protocol.Room
import boards.protocol.Room.{Player, PlayerIdentity, RichPlayer, RichRoom, Status}
import boards.protocol.UserProtocol.User
import boards.dsl.meta.TurnId.next
import models.GameModel
import org.apache.pekko.actor.{Status as _, *}
import schema.RoomTable.rooms
import schema.UserTable.UserRow
import slick.jdbc.H2Profile.api.Database
import slick.lifted.Functions.user

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.DurationInt
import scala.util.Random

class RoomActor
  (roomId: String, system: ActorRef)
  (using Database)
extends Actor:
  
  given ExecutionContext = context.system.dispatcher
  
  val subscribers: mutable.Map[ActorRef, Subscriber] = mutable.Map.empty
  
  var room: RichRoom = Room.empty
  var state: GameState = GameState.empty
  
  self ! ReloadRoom
  self ! ReloadState
  
  /** Provide all active clients with the latest information so that the scene may be updated. */
  def render (): Unit =
    subscribers.values.foreach(render)
      
  /** Provide a given client with the latest information so that the scene may be updated. */
  def render (sub: Subscriber): Unit =
  
    // If the client is currently viewing an old state, we need to provide them with this instead.
    val stateAtTime = sub.turnId match
      case Some(turnId) => state.atTime((turnId + state.turnId.next) % (state.turnId.next)).inert
      case None => state
      
    // Construct a scene summarising all relevant information and send it to the client.
    sub.session ! GameResponse.Render(Scene(room, sub.user.map(_.userId), stateAtTime, state))
    
  private def activePlayer: RichPlayer =
    room.players(state.activePlayer.toInt)
  
  private def canTakeAction (userId: Int): Boolean =
    room.isActive &&
    room.playersOf(userId).map(_.position).contains(state.activePlayer) &&
    !room.player(state.activePlayer).hasResigned
  
  def receive =
    
    case ReloadRoom =>
      for room <- GameModel().getRichRoom(roomId) do
        this.room = room
        render()
    
    case ReloadState =>
      for state <- GameModel().getGameState(roomId) do
        this.state = state
        render()
    
    case Subscribe(me, out) =>
      val sub = Subscriber(out, me)
      subscribers += out -> sub
      render(sub)
    
    case Act(me, out, ViewTurnId(turnId)) =>
      if room.status.isComplete then
        val sub = subscribers(out).copy(turnId = Some(turnId))
        subscribers += out -> sub
        render(sub)
      
    case Act(Some(me), out, TakeAction(inputId)) =>
      for
        result <- state.applyInputById(inputId)
        if canTakeAction(me)
      do for
        _ <- GameModel().takeAction(roomId, inputId, result.isFinal)
      do
        this.state = result
        if result.isFinal then
          this.room = this.room.withStatus(Status.Complete)
        render()
        if room.isActive && activePlayer.isBot then
          self ! PlayAsBot
    
    case PlayAsBot =>
      activePlayer.asBotOpt
        .map(_.botId).map(BotCatalogue.byId.apply)
        .foreach: bot =>
          val input   = bot.choose(state)
          val result  = state.applyInput(input).get
          val inputId = state.inputs.indexOf(input)
          GameModel().takeAction(roomId, inputId, result.isFinal).foreach: _ =>
            this.state = result
            if result.isFinal then
              this.room = this.room.withStatus(Status.Complete)
            render()
            if room.isActive && activePlayer.isBot then
              self ! PlayAsBot
      
    case Act(Some(me), out, InviteToRoom(user)) => ???
    
    case Act(Some(me), out, RemovePlayers(positions*)) =>
      if room.isParticipating(me) && room.isPending then
        for _ <- GameModel().leaveRoom(roomId)(positions*) do
          self ! ReloadRoom
          if room.forkedFrom.isEmpty then self ! ReloadState
          if room.rematchOf.nonEmpty then system ! SystemActor.Protocol.ReloadRoom(room.rematchOf.get.id)
    
    case Act(Some(me), out, SwapPlayers(left, right)) =>
      if room.isParticipating(me) && room.isPending then
        for _ <- GameModel().swapPlayers(roomId)(left, right) do
          self ! ReloadRoom
      
    case Act(Some(me), out, PromotePlayer(user)) => ???
    
    case Act(Some(me), out, ChangeGame(game)) => ???
    
    case Act(Some(me), out, SetProperty(property, value)) =>
      if room.isParticipating(me) && room.isPending then
        for _ <- GameModel().setProperty(roomId, property, value) do
          self ! ReloadRoom
          self ! ReloadState
    
    case Act(Some(me), out, JoinRoom(multiplicity)) =>
      if room.isPending then
        for _ <- GameModel().joinRoom(roomId, me, multiplicity) do
          self ! ReloadRoom
          if room.forkedFrom.isEmpty then self ! ReloadState
          if room.rematchOf.nonEmpty then system ! SystemActor.Protocol.ReloadRoom(room.rematchOf.get.id)
    
    case Act(Some(me), out, AddBot(id)) =>
      if room.isPending && room.userIsPlaying(me) then
        for _ <- GameModel().addBot(room.id, id) do
          self ! ReloadRoom
          if room.forkedFrom.isEmpty then self ! ReloadState
          if room.rematchOf.nonEmpty then system ! SystemActor.Protocol.ReloadRoom(room.rematchOf.get.id)
    
    case Act(Some(me), out, StartGame) =>
      if room.isParticipating(me) then
        for _ <- GameModel().startGame(roomId) do
          self ! ReloadRoom
          if activePlayer.isBot then
            self ! PlayAsBot
    
    case Act(Some(me), out, CancelRoom) => ???
    
    case Act(Some(me), out, Resign(resign, positions*)) =>
      for
        players <- GameModel().getPlayers(roomId)
        // Ensure user can't resign on behalf of a player on another device.
        myPositions = players.filter(_.userIdOpt.contains(me)).map(_.position).filter(positions.contains)
        _ <- GameModel().resign(roomId, resign)(myPositions*)
      do self ! ReloadRoom
    
    case Act(Some(me), out, OfferDraw(draw, positions*)) =>
      for
        players <- GameModel().getPlayers(roomId)
        // Ensure user can't offer draw on behalf of a player on another device.
        myPositions = players.filter(_.userIdOpt.contains(me)).map(_.position).filter(positions.contains)
        _ <- GameModel().offerDraw(roomId, draw)(myPositions*)
      do self ! ReloadRoom
    
    case Act(Some(me), out, OfferRematch) =>
      for rematch <- GameModel().createOrJoinRematch(me, roomId) do
        out ! GameResponse.Goto(rematch.id)
        self ! ReloadRoom
        system ! SystemActor.Protocol.ReloadRoom(rematch.id)
    
    case Act(Some(me), out, ForkState(turnId)) =>
      for room <- GameModel().forkRoom(me, roomId, turnId.getOrElse(state.turnId)) do
        out ! GameResponse.Goto(room.id)
        
object RoomActor:
  
  def props (roomId: String, system: ActorRef) (using Database) =
    Props(RoomActor(roomId, system))
    
  enum Protocol:
    case Subscribe (user: Option[User], out: ActorRef)
    case Act (userId: Option[Int], out: ActorRef, request: GameRequest)
    case ReloadRoom
    case ReloadState
    case PlayAsBot
    
  /**
   * An active subscription for a user who is viewing or participating in this room.
   *
   * @param session The websocket session through with which we may communicate with the client.
   * @param user The user who is logged in on this device, if any.
   * @param turnId The ID of the turn that the user is currently viewing, or else None if the latest.
   */
  case class Subscriber (
    session: ActorRef,
    user: Option[User],
    turnId: Option[TurnId] = None,
  )