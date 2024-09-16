package controllers

import boards.games.Chess
import boards.graphics.Scene
import boards.protocol.GameProtocol.*
import boards.protocol.GameProtocol.GameRequest.*
import boards.imports.circe.{*, given}
import org.apache.pekko.actor.{*, given}
import org.apache.pekko.pattern.ask
import org.apache.pekko.util.Timeout
import schema.RoomTable.RoomRow
import schema.UserTable.UserRow

import scala.compiletime.ops.boolean.!
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*

class SessionActor (
  out: ActorRef,
  system: ActorRef,
  roomId: String,
  userId: Option[Int]
) extends Actor:
  
  given ExecutionContext = context.system.dispatcher
  given Timeout = 5.seconds
  
  val roomActor = (system ? roomId).mapTo[ActorRef]
  roomActor.map(_ ! RoomActor.Protocol.Subscribe)
  
  def receive =
    case message: String =>
      decode[GameRequest](message).toOption.map: request =>
        userId.map: userId =>
          roomActor.map(_ ! RoomActor.Protocol.Update(userId, request))
    case update: Scene =>
      out ! update

object SessionActor:
  def props(out: ActorRef, system: ActorRef, roomId: String, userId: Option[Int]) =
    Props(SessionActor(out, system, roomId, userId))