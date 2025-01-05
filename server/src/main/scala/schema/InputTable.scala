package schema

import InputTable.{*, given}
import boards.dsl.meta.TurnId
import boards.dsl.meta.TurnId.TurnId
import com.sun.org.apache.xalan.internal.lib.ExsltDatetime.time
import slick.jdbc.H2Profile.api.*

class InputTable(tag: Tag) extends Table[InputRow](tag, "INPUTS"):
  
  def roomId = column[String]("ROOM_ID", O.PrimaryKey)
  def turnId = column[Int]("TURN_ID", O.PrimaryKey)
  def userId = column[Int]("USER_ID")
  def inputId = column[Int]("INPUT_ID")
  def timestamp = column[Long]("TIMESTAMP")
  
  def * = (roomId, turnId, userId, inputId, timestamp).mapTo[InputRow]
  
  def roomFk = foreignKey("ROOM_FK", roomId, RoomTable.rooms)(_.id)
  def userFk = foreignKey("USER_FK", userId, UserTable.users)(_.id)

object InputTable:
  
  case class InputRow(roomId: String, turnId: Int, userId: Int, inputId: Int, timestamp: Long)
  
  def inputs = TableQuery[InputTable]