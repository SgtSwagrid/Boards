package schema

import InputTable.*
import com.sun.org.apache.xalan.internal.lib.ExsltDatetime.time
import slick.jdbc.H2Profile.api.*

class InputTable(tag: Tag) extends Table[InputRow](tag, "INPUTS"):
  def roomId = column[String]("ROOM_ID", O.PrimaryKey)
  def position = column[Int]("POSITION", O.PrimaryKey)
  def userId = column[Int]("USER_ID")
  def inputId = column[Int]("INPUT_ID")
  def timestamp = column[Long]("TIMESTAMP")
  def * = (roomId, position, userId, inputId, timestamp).mapTo[InputRow]
  def room = foreignKey("ROOM_FK", roomId, RoomTable.rooms)(_.id)
  def user = foreignKey("USER_FK", userId, UserTable.users)(_.id)

object InputTable:
  case class InputRow(roomId: String, position: Int, userId: Int, inputId: Int, timestamp: Long)
  def actions = TableQuery[InputTable]