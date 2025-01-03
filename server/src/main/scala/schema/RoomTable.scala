package schema

import RoomTable.{*, given}
import boards.protocol.Room
import boards.protocol.Room.Status
import slick.jdbc.H2Profile.api.*

class RoomTable(tag: Tag) extends Table[Room](tag, "ROOMS"):
  def id = column[String]("ID", O.PrimaryKey)
  def gameId = column[String]("GAME_ID")
  def status = column[Status]("STATUS")
  def properties = column[Map[String, Int]]("PROPERTIES")
  
  def * = (id, gameId, status, properties).mapTo[Room]
  
object RoomTable:
  
  def rooms = TableQuery[RoomTable]
  
  given BaseColumnType[Status] = MappedColumnType.base[Status, String] (
    (status: Status) => status.toString.toUpperCase,
    (status: String) => Status.values.find(_.toString.toUpperCase == status.toUpperCase).get,
  )
  
  given BaseColumnType[Map[String, Int]] = MappedColumnType.base[Map[String, Int], String] (
    (properties: Map[String, Int]) => properties.map((k, v) => s"$k=$v").mkString(","),
    (properties: String) => properties.split(",").filter(_.nonEmpty).map(_.split("=").toSeq)
      .map{ case Seq(k, v) => k -> v.toInt }.toMap
  )