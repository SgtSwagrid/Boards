package schema

import PlayerTable.*
import slick.jdbc.H2Profile.api.*

class PlayerTable(tag: Tag) extends Table[PlayerRow](tag, "PLAYERS"):
  def userId = column[Int]("USER_ID", O.PrimaryKey)
  def roomId = column[String]("ROOM_ID", O.PrimaryKey)
  def position = column[Int]("POSITION")
  def isOwner = column[Boolean]("IS_OWNER")
  def * = (userId, roomId, position, isOwner).mapTo[PlayerRow]
  def user = foreignKey("USER_FK", userId, UserTable.users)(_.id)
  def room = foreignKey("ROOM_FK", roomId, RoomTable.rooms)(_.id)
  
object PlayerTable:
  case class PlayerRow(userId: Int, roomId: String, position: Int, isOwner: Boolean)
  def players = TableQuery[PlayerTable]