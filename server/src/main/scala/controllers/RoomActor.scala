package controllers

import boards.Games
import RoomActor.Protocol.*
import boards.algebra.state.GameState.NonFinalState
import boards.algebra.state.InstantaneousState.given
import boards.algebra.state.GameState
import boards.graphics.Scene
import boards.protocol.GameProtocol.*
import boards.protocol.GameProtocol.GameRequest.*
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
  
  case class Subscriber(session: ActorRef, spectator: Participant)
  
  val subscribers: mutable.Set[Subscriber] = mutable.Set.empty
  
  var room: Room = Room.empty
  var state: GameState = GameState.empty
  var players: Seq[Player] = Seq.empty
  var status: Status = Status.Pending
  
  for
    room <- GameModel().getRoomById(roomId)
    state <- GameModel().getGameState(roomId)
    players <- GameModel().getPlayers(roomId)
  yield
    this.room = room.get
    this.state = state
    this.players = players
    render()
  
  updatePlayers()
  
  private def canTakeAction(spectator: Participant): Boolean =
    room.status.isActive && spectator.isPlayer(state.activePlayer)
  
  def receive =
    case Subscribe(me, out) =>
      
      for participant <- GameModel().getParticipant(roomId, me) do
        subscribers += Subscriber(out, participant)
        out ! Scene(state, players, room, participant)
      
    case Update(me, TakeAction(hash)) =>
      for
        _ <- Option.when(canTakeAction(me))(())
        userId <- me.userIdOption
        result <- state.takeActionByHash(hash)
      do for
        _ <- GameModel().takeAction(roomId, userId, hash)
      do
        this.state = result
        render()
      
    case Update(me, InviteToRoom(user)) => ???
    
    case Update(me, RemovePlayers(positions*)) =>
      if me.isPlaying then
        println(positions)
        for _ <- GameModel().leaveRoom(roomId)(positions*)
        do updatePlayers()
    
    case Update(me, ReorderPlayer(player)) =>
      if me.isPlaying then
        for _ <- GameModel().reorderPlayer(roomId, player)
        do updatePlayers()
      
    case Update(me, PromotePlayer(user)) => ???
    case Update(me, ChangeGame(game)) => ???
    
    case Update(me, JoinRoom) =>
      for userId <- me.userIdOption
      do for _ <- GameModel().joinRoom(roomId, userId)
      do updatePlayers()
    
    case Update(me, StartGame) =>
      if me.isPlaying then
        for room <- GameModel().startGame(roomId) do
          this.room = room
          render()
    
    case Update(me, CancelRoom) => ???
    
  def updatePlayers() =
    for players <- GameModel().getPlayers(roomId) do
      this.players = players
      val subs = subscribers.map:
        case Subscriber(session, participant: RegisteredParticipant) =>
          Subscriber(session, participant.copy(
            positions = players.filter(_.userId == participant.userId).map(_.position)
          ))
        case sub => sub
      subscribers.clear()
      subscribers ++= subs
      render()
    
  def render() =
    subscribers.foreach(sub => sub.session ! Scene(state, players, room, sub.spectator))
  
object RoomActor:
  
  def props(roomId: String)(using Database) =
    Props(RoomActor(roomId))
    
  enum Protocol:
    case Subscribe(userId: Option[Int], out: ActorRef)
    case Update(spectator: Participant, request: GameRequest)