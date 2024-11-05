package schema

import RoomTable.{*, given}
import boards.protocol.Room
import boards.protocol.Room.Status
import slick.jdbc.H2Profile.api.*

class RoomTable(tag: Tag) extends Table[Room](tag, "ROOMS"):
  def id = column[String]("ID", O.PrimaryKey)
  def gameId = column[String]("GAME_ID")
  def status = column[Status]("STATUS")
  
  def * = (id, gameId, status).mapTo[Room]
  
object RoomTable:
  
  def rooms = TableQuery[RoomTable]
  
  given BaseColumnType[Status] = MappedColumnType.base[Status, String] (
    (status: Status) => status.toString.toUpperCase,
    (status: String) => Status.values.find(_.toString.toUpperCase == status.toUpperCase).get,
  )