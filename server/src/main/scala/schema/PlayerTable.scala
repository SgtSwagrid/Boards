package schema

import PlayerTable.*
import slick.jdbc.H2Profile.api.*

class PlayerTable(tag: Tag) extends Table[PlayerRow](tag, "PLAYERS"):
  
  def userId = column[Int]("USER_ID")
  def roomId = column[String]("ROOM_ID", O.PrimaryKey)
  def position = column[Int]("POSITION", O.PrimaryKey)
  def isOwner = column[Boolean]("IS_OWNER")
  def hasResigned = column[Boolean]("HAS_RESIGNED")
  def hasOfferedDraw = column[Boolean]("HAS_OFFERED_DRAW")
  
  def * = (userId, roomId, position, isOwner, hasResigned, hasOfferedDraw).mapTo[PlayerRow]
  
  def userFk = foreignKey("USER_FK", userId, UserTable.users)(_.id)
  def roomFk = foreignKey("ROOM_FK", roomId, RoomTable.rooms)(_.id)
  
object PlayerTable:
  
  case class PlayerRow (
    userId: Int,
    roomId: String,
    position: Int,
    isOwner: Boolean,
    hasResigned: Boolean = false,
    hasOfferedDraw: Boolean = false,
  )
  
  def players = TableQuery[PlayerTable]