package schema

import GameTable.*
import slick.jdbc.H2Profile.api.*

class GameTable(tag: Tag) extends Table[GameRow](tag, "GAMES"):
  def id = column[Int]("ID", O.PrimaryKey, O.AutoInc)
  def game = column[String]("GAME")
  def state = column[String]("STATE")
  def * = (id, game, state).mapTo[GameRow]

object GameTable:
  case class GameRow(id: Int, game: String, state: String)
  def games = TableQuery[GameTable]