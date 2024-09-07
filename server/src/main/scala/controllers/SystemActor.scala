package controllers

import org.apache.pekko.actor.*

import scala.collection.mutable
import schema.RoomTable.RoomRow
import slick.jdbc.H2Profile.api.Database

class SystemActor(using Database) extends Actor:
  
  val rooms: mutable.Map[String, ActorRef] = mutable.Map.empty
  
  def receive =
    case roomId: String =>
      lazy val actor = context.actorOf(RoomActor.props(roomId), s"room-${roomId}")
      val roomActor = rooms.getOrElseUpdate(roomId, actor)
      sender() ! roomActor
  
object SystemActor:
  
  def props(using Database) = Props(new SystemActor)