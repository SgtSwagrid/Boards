package schema

import RoomTable.{*, given}
import boards.dsl.meta.TurnId
import boards.dsl.meta.TurnId.TurnId
import boards.protocol.Room
import boards.protocol.Room.Status
import slick.jdbc.H2Profile.api.*

class RoomTable(tag: Tag) extends Table[Room](tag, "ROOMS"):
  
  def id = column[String]("ID", O.PrimaryKey)
  def gameId = column[String]("GAME_ID")
  def status = column[Status]("STATUS")
  def properties = column[Map[String, Int]]("PROPERTIES")
  def forkedFrom = column[Option[String]]("FORKED_FROM")
  def forkedTurn = column[Option[TurnId]]("FORKED_TURN")
  def rematchOf = column[Option[String]]("REMATCH_OF")
  def rematch = column[Option[String]]("REMATCH")
  
  def * = (id, gameId, status, properties, forkedFrom, forkedTurn, rematchOf, rematch).mapTo[Room]
  
  def forkedFromFk = foreignKey("FORKED_FROM_FK", forkedFrom, RoomTable.rooms)(_.id.?)
  def rematchOfFk = foreignKey("REMATCH_OF_FK", rematchOf, RoomTable.rooms)(_.id.?)
  def rematchFk = foreignKey("REMATCH_FK", rematchOf, RoomTable.rooms)(_.id.?)
  
object RoomTable:
  
  def rooms = TableQuery[RoomTable]
  
  given BaseColumnType[TurnId] = MappedColumnType.base[TurnId, Int](
    (turnId: TurnId) => turnId.toInt,
    (turnId: Int) => TurnId(turnId)
  )
  
  given BaseColumnType[Status] = MappedColumnType.base[Status, String] (
    (status: Status) => status.toString.toUpperCase,
    (status: String) => Status.values.find(_.toString.toUpperCase == status.toUpperCase).get,
  )
  
  given BaseColumnType[Map[String, Int]] = MappedColumnType.base[Map[String, Int], String] (
    (properties: Map[String, Int]) => properties.map((k, v) => s"$k=$v").mkString(","),
    (properties: String) => properties.split(",").filter(_.nonEmpty).map(_.split("=").toSeq)
      .map{ case Seq(k, v) => k -> v.toInt }.toMap
  )