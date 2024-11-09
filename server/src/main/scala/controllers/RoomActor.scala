package controllers

import boards.Games
import RoomActor.Protocol.*
import boards.algebra.state.GameState.NonFinalState
import boards.algebra.state.InstantaneousState.given
import boards.algebra.state.GameState
import boards.graphics.Scene
import boards.protocol.GameProtocol.*
import boards.protocol.GameProtocol.GameRequest.*
import boards.protocol.Room
import boards.protocol.Room.{Player, Status, RichRoom}
import boards.protocol.UserProtocol.User
import models.GameModel
import org.apache.pekko.actor.{Status as _, *}
import schema.RoomTable.rooms
import schema.UserTable.UserRow
import slick.jdbc.H2Profile.api.Database
import slick.lifted.Functions.user

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

class RoomActor
  (roomId: String)
  (using Database)
extends Actor:
  
  given ExecutionContext = context.system.dispatcher
  
  case class Subscriber(session: ActorRef, user: Option[User])
  
  val subscribers: mutable.Set[Subscriber] = mutable.Set.empty
  
  var room: RichRoom = Room.empty
  var state: GameState = GameState.empty
  
  for
    room <- GameModel().getRoomById(roomId)
    state <- GameModel().getGameState(roomId)
    players <- GameModel().getPlayers(roomId)
  do
    this.room = room.get.withPlayers(players)
    this.state = state
    render()
  
  updatePlayers()
  
  private def canTakeAction(userId: Int): Boolean =
    room.isActive &&
    room.playersOf(userId).map(_.position).contains(state.activePlayer) &&
    !room.players(state.activePlayer).hasResigned
  
  def receive =
    
    case Subscribe(me, out) =>
      subscribers += Subscriber(out, me)
      out ! Scene(room, me.map(_.userId), state)
    
    case ViewState(time, me, out) =>
      if room.status.isComplete then
        state.atTime((time + (state.time + 1)) % (state.time + 1)).foreach: state =>
          out ! Scene(room, me, state)
      
    case Update(me, TakeAction(hash)) =>
      for
        result <- state.takeActionByHash(hash)
        if canTakeAction(me)
      do for
        _ <- GameModel().takeAction(roomId, me, hash, result.isFinal)
      do
        this.state = result
        if result.isFinal then
          this.room = this.room.withStatus(Status.Complete)
        render()
      
    case Update(me, InviteToRoom(user)) => ???
    
    case Update(me, RemovePlayers(positions*)) =>
      if room.isParticipating(me) then
        for _ <- GameModel().leaveRoom(roomId)(positions*)
        do updatePlayers()
    
    case Update(me, SwapPlayers(left, right)) =>
      if room.isParticipating(me) then
        for _ <- GameModel().swapPlayers(roomId)(left, right)
        do updatePlayers()
      
    case Update(me, PromotePlayer(user)) => ???
    case Update(me, ChangeGame(game)) => ???
    
    case Update(me, JoinRoom) =>
      for _ <- GameModel().joinRoom(roomId, me)
      do updatePlayers()
    
    case Update(me, StartGame) =>
      if room.isParticipating(me) then
        for room <- GameModel().startGame(roomId) do
          this.room = room.withPlayers(this.room.simplePlayers)
          render()
    
    case Update(me, CancelRoom) => ???
    
    case Update(me, Resign(resign, positions*)) =>
      for
        players <- GameModel().getPlayers(roomId)
        // Ensure user can't resign on behalf of a player on another device.
        myPositions = players.filter(_.userId == me).map(_.position).filter(positions.contains)
        _ <- GameModel().resign(roomId, resign)(myPositions*)
        room <- GameModel().getRoomById(roomId).map(_.get)
        players <- GameModel().getPlayers(roomId)
      do
        this.room = room.withPlayers(players)
        render()
    
    case Update(me, OfferDraw(draw, positions*)) =>
      for
        players <- GameModel().getPlayers(roomId)
        // Ensure user can't offer draw on behalf of a player on another device.
        myPositions = players.filter(_.userId == me).map(_.position).filter(positions.contains)
        _ <- GameModel().offerDraw(roomId, draw)(myPositions*)
        room <- GameModel().getRoomById(roomId).map(_.get)
        players <- GameModel().getPlayers(roomId)
      do
        this.room = room.withPlayers(players)
        render()
    
  def updatePlayers() =
    for players <- GameModel().getPlayers(roomId) do
      this.room = this.room.withPlayers(players)
      render()
    
  def render() =
    subscribers.foreach(sub => sub.session ! Scene(room, sub.user.map(_.userId), state))
  
object RoomActor:
  
  def props(roomId: String)(using Database) =
    Props(RoomActor(roomId))
    
  enum Protocol:
    case Subscribe(user: Option[User], out: ActorRef)
    case Update(userId: Int, request: GameRequest)
    case ViewState(time: Int, user: Option[Int], out: ActorRef)