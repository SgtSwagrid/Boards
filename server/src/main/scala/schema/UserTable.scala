package schema

import UserTable.*
import slick.jdbc.H2Profile.api.*

class UserTable(tag: Tag) extends Table[UserRow](tag, "USERS"):
  def id = column[Int]("ID", O.PrimaryKey, O.AutoInc)
  def username = column[String]("USERNAME")
  def email = column[String]("EMAIL")
  def passwordHash = column[String]("PASSWORD_HASH")
  def * = (id, username, email, passwordHash).mapTo[UserRow]

object UserTable:
  case class UserRow(id: Int, username: String, email: String, passwordHash: String)
  def users = TableQuery[UserTable]