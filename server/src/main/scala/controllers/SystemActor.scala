package controllers

import org.apache.pekko.actor.*

import scala.collection.mutable
import slick.jdbc.H2Profile.api.Database
import SystemActor.Protocol.*

class SystemActor(using Database) extends Actor:
  
  val rooms: mutable.Map[String, ActorRef] = mutable.Map.empty
  
  def receive =
    
    case OpenRoom(roomId) =>
      lazy val actor = context.actorOf(RoomActor.props(roomId, self), s"room-${roomId}")
      val roomActor = rooms.getOrElseUpdate(roomId, actor)
      sender() ! roomActor
      
    case CloseRoom(roomId) =>
      ???
      
    case ReloadRoom(roomId) =>
      rooms.get(roomId).foreach: room =>
        room ! RoomActor.Protocol.ReloadRoom
  
object SystemActor:
  
  def props(using Database) = Props(new SystemActor)
  
  enum Protocol:
    case OpenRoom(roomId: String)
    case CloseRoom(roomId: String)
    case ReloadRoom(roomId: String)