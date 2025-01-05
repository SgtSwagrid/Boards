package controllers

import boards.graphics.Scene
import boards.protocol.GameProtocol.*
import boards.protocol.GameProtocol.GameRequest.*
import boards.imports.circe.{*, given}
import boards.protocol.UserProtocol.User
import models.GameModel
import org.apache.pekko.actor.{*, given}
import org.apache.pekko.pattern.ask
import org.apache.pekko.util.Timeout
import schema.UserTable.UserRow

import scala.compiletime.ops.boolean.!
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import slick.jdbc.H2Profile.api.Database

class SessionActor (
  out: ActorRef,
  system: ActorRef,
  roomId: String,
  user: Option[User]
) (using Database) extends Actor:
  
  given ExecutionContext = context.system.dispatcher
  given Timeout = 5.seconds
  
  val roomActor = (system ? SystemActor.Protocol.OpenRoom(roomId)).mapTo[ActorRef]
  roomActor.map(_ ! RoomActor.Protocol.Subscribe(user, out))
  
  def receive =
    case message: String =>
      decode[GameRequest](message).toOption.foreach: request =>
        roomActor.map[Unit](_ ! RoomActor.Protocol.Act(user.map(_.userId), out, request))

object SessionActor:
  def props(out: ActorRef, system: ActorRef, roomId: String, user: Option[User])(using Database) =
    Props(SessionActor(out, system, roomId, user))