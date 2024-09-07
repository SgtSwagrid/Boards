package models

import boards.protocol.UserProtocol
import boards.protocol.UserProtocol.*
import cats.Monad
import cats.data.EitherT
import schema.UserTable
import schema.UserTable.{UserRow, users}
import org.mindrot.jbcrypt.BCrypt
import slick.dbio.{DBIO, DBIOAction}
import slick.jdbc.H2Profile.api.*

import scala.concurrent.{ExecutionContext, Future}

class UserModel(using db: Database, ec: ExecutionContext):
  
  def getUserById(id: Int): Future[Option[UserRow]] =
    db.run(UserTable.users.filter(_.id === id).result.headOption)
    
  def login(form: LoginForm): Future[LoginResponse] =
    
    import LoginError.*
    val LoginForm(username, password) = form
    
    val action = userByName(username).map: user =>
      for
        user <- user.toRight(InvalidUsername)
        _ <- Either.cond(BCrypt.checkpw(password, user.passwordHash), (), InvalidPassword)
      yield user.id
    
    db.run(action)
  
  def register(form: RegistrationForm): Future[RegistrationResponse] =
    
    val action = for
      validated <- validateRegistration(form)
      user <- validated match
        case Left(error) => DBIO.successful(Left(error))
        case Right(_) => createUser(form).map(Right.apply)
    yield user
    
    db.run(action)
  
  private def validateRegistration(form: RegistrationForm): DBIO[Either[RegistrationError, Unit]] =
    
    import RegistrationError.*
    val RegistrationForm(username, email, password) = form
    
    for
      usernameExists <- usernameExists(username)
      emailExists <- emailExists(email)
    yield for
      _ <- form.validate
      _ <- Either.cond(!usernameExists, (), UsernameTaken(username))
      _ <- Either.cond(!emailExists, (), EmailTaken(email))
    yield ()
  
  private def userByName(username: String): DBIO[Option[UserRow]] =
    UserTable.users.filter(_.username.toLowerCase === username.toLowerCase)
      .result.map(_.headOption)
  
  private def usernameExists(username: String): DBIO[Boolean] =
    userByName(username).map(_.nonEmpty)
  
  private def emailExists(email: String): DBIO[Boolean] =
    UserTable.users.filter(_.email.toLowerCase === email.toLowerCase)
      .result.map(_.nonEmpty)
    
  private def createUser(form: RegistrationForm): DBIO[Int] =
    val RegistrationForm(username, email, password) = form
    UserTable.users += UserRow(-1, username, email, hashPassword(password))
    
  private def hashPassword(password: String): String =
    BCrypt.hashpw(password, BCrypt.gensalt())