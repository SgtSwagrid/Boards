package schema

import ActionTable.*
import com.sun.org.apache.xalan.internal.lib.ExsltDatetime.time
import slick.jdbc.H2Profile.api.*

class ActionTable(tag: Tag) extends Table[ActionRow](tag, "ACTIONS"):
  def roomId = column[String]("ROOM_ID", O.PrimaryKey)
  def position = column[Int]("POSITION", O.PrimaryKey)
  def userId = column[Int]("USER_ID")
  def actionHash = column[String]("ACTION_HASH")
  def timestamp = column[Long]("TIMESTAMP")
  def * = (roomId, position, userId, actionHash, timestamp).mapTo[ActionRow]
  def room = foreignKey("ROOM_FK", roomId, RoomTable.rooms)(_.id)
  def user = foreignKey("USER_FK", userId, UserTable.users)(_.id)

object ActionTable:
  case class ActionRow(roomId: String, position: Int, userId: Int, actionHash: String, timestamp: Long)
  def actions = TableQuery[ActionTable]