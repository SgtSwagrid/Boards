package schema

import PlayerTable.*
import slick.jdbc.H2Profile.api.*

class PlayerTable(tag: Tag) extends Table[PlayerRow](tag, "PLAYERS"):
  def userId = column[Int]("USER_ID", O.PrimaryKey)
  def gameId = column[Int]("GAME_ID", O.PrimaryKey)
  def position = column[Int]("POSITION")
  def isOwner = column[Boolean]("IS_OWNER")
  def * = (userId, gameId, position, isOwner).mapTo[PlayerRow]
  def user = foreignKey("USER_FK", userId, UserTable.users)(_.id)
  def game = foreignKey("GAME_FK", gameId, GameTable.games)(_.id)
  
object PlayerTable:
  case class PlayerRow(userId: Int, gameId: Int, position: Int, isOwner: Boolean)
  def players = TableQuery[PlayerTable]