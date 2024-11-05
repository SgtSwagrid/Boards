package controllers

import boards.games.Chess
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
  
  val roomActor = (system ? roomId).mapTo[ActorRef]
  roomActor.map(_ ! RoomActor.Protocol.Subscribe(user, out))
  
  def receive =
    case message: String =>
      for
        userId <- user.map(_.userId)
        request <- decode[GameRequest](message).toOption
      do roomActor.map(_ ! RoomActor.Protocol.Update(userId, request))

object SessionActor:
  def props(out: ActorRef, system: ActorRef, roomId: String, user: Option[User])(using Database) =
    Props(SessionActor(out, system, roomId, user))