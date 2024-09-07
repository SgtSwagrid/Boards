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
import org.apache.pekko.actor.*
import schema.RoomTable.{RoomRow, rooms}
import schema.UserTable.UserRow
import slick.jdbc.H2Profile.api.Database

import scala.collection.mutable
import scala.concurrent.ExecutionContext

class RoomActor
  (roomId: String)
  (using Database)
extends Actor:
  
  given ExecutionContext = context.system.dispatcher
  
  val subscribers: mutable.Set[ActorRef] = mutable.Set.empty
  
  var state: GameState = GameState.empty
  var players: Seq[Player] = Seq.empty
  
  for
    state <- GameModel().getGameState(roomId)
    players <- GameModel().getPlayers(roomId)
  do
    this.state = state
    this.players = players
    render()
  
  updatePlayers()
  
  def receive =
    case Subscribe =>
      subscribers += sender()
      sender() ! Scene(state, players)
      
    case Update(me, TakeAction(hash)) =>
      for
        _ <- Option.when(state.activePlayer == me)(())
        state <- state.takeActionByHash(hash)
      do for
        _ <- GameModel().takeAction(roomId, me, hash)
      do
        this.state = state
        render()
      
    case Update(me, InviteToRoom(user)) => ???
    case Update(me, RemovePlayer(user)) => ???
    case Update(me, ReorderPlayer(user, pos)) => ???
    case Update(me, PromotePlayer(user)) => ???
    case Update(me, ChangeGame(game)) => ???
    
    case Update(me, JoinRoom) =>
      for
        _ <- GameModel().joinRoom(me, roomId)
        _ <- updatePlayers()
      do {}
    
    case Update(me, StartGame) => ???
    case Update(me, CancelRoom) => ???
    
  def updatePlayers() =
    for
      players <- GameModel().getPlayers(roomId)
    yield
      this.players = players
      render()
    
  def render() =
    subscribers.foreach(_ ! Scene(state, players))
    
  
object RoomActor:
  
  def props(roomId: String)(using Database) =
    Props(RoomActor(roomId))
    
  enum Protocol:
    case Subscribe
    case Update(userId: Int, request: GameRequest)