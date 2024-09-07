package schema

import RoomTable.*
import slick.jdbc.H2Profile.api.*

class RoomTable(tag: Tag) extends Table[RoomRow](tag, "ROOMS"):
  def id = column[String]("ID", O.PrimaryKey)
  def gameId = column[Int]("GAME_ID")
  def status = column[Status]("STATUS")
  
  given BaseColumnType[Status] = MappedColumnType.base[Status, String] (
    (status: Status) => status.toString.toUpperCase,
    (status: String) => Status.values.find(_.toString.toUpperCase == status.toUpperCase).get,
  )
  def * = (id, gameId, status).mapTo[RoomRow]
  
object RoomTable:
  case class RoomRow(id: String, gameId: Int, status: Status)
  enum Status:
    case Pending, Active, Complete
  def rooms = TableQuery[RoomTable]