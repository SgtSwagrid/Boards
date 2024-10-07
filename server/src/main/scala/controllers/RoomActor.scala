package controllers

import boards.Games
import RoomActor.Protocol.*
import boards.algebra.GameState
import boards.algebra.GameState.NonFinalState
import boards.algebra.InstantaneousState.given
import boards.graphics.Scene
import boards.protocol.GameProtocol.*
import boards.protocol.GameProtocol.GameRequest.*
import models.GameModel
import org.apache.pekko.actor.{Status => _, *}
import schema.RoomTable.rooms
import schema.UserTable.UserRow
import slick.jdbc.H2Profile.api.Database

import scala.collection.mutable
import scala.concurrent.ExecutionContext

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
  do
    this.room = room.get
    this.state = state
    this.players = players
    render()
  
  updatePlayers()
  
  private def canTakeAction(spectator: Participant): Boolean =
    room.status.isActive &&
      spectator.isActivePlayer(state.activePlayer)
  
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
    
    case Update(me, RemovePlayer(user)) =>
      for _ <- GameModel().leaveRoom(roomId, user) do
        updatePlayers()
    
    case Update(me, ReorderPlayer(user)) =>
      for _ <- GameModel().reorderPlayer(roomId, user) do
        updatePlayers()
      
    case Update(me, PromotePlayer(user)) => ???
    case Update(me, ChangeGame(game)) => ???
    
    case Update(me, JoinRoom) =>
      for
        userId <- me.userIdOption
      do for
        _ <- GameModel().joinRoom(roomId, userId)
      do
        updatePlayers()
    
    case Update(me, StartGame) =>
      for room <- GameModel().startGame(roomId) do
        this.room = room
        render()
    
    case Update(me, CancelRoom) => ???
    
  def updatePlayers() =
    for players <- GameModel().getPlayers(roomId) do
      this.players = players
      val playersById = players.map(p => p.userId -> p).toMap
      val subs = subscribers.map:
        case Subscriber(session, Spectator(id, _)) if playersById.contains(id) =>
          Subscriber(session, playersById(id))
        case Subscriber(session, Player(id, _, _, _, name)) if !playersById.contains(id) =>
          Subscriber(session, Spectator(id, name))
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