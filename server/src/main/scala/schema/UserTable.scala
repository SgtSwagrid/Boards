package schema

import UserTable.*
import boards.protocol.UserProtocol.User
import slick.jdbc.H2Profile.api.*

class UserTable(tag: Tag) extends Table[UserRow](tag, "USERS"):
  def id = column[Int]("ID", O.PrimaryKey, O.AutoInc)
  def username = column[String]("USERNAME")
  def email = column[String]("EMAIL")
  def passwordHash = column[String]("PASSWORD_HASH")
  def joined = column[Long]("JOINED")
  def * = (id, username, email, passwordHash, joined).mapTo[UserRow]

object UserTable:
  case class UserRow(id: Int, username: String, email: String, passwordHash: String, joined: Long):
    def toUser = User(id, username, joined)
  def users = TableQuery[UserTable]