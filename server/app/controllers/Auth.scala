package controllers

import boards.protocol.UserProtocol.{LoginError, LoginForm, LoginResponse, RegistrationForm, RegistrationResponse, User}
import com.fasterxml.jackson.annotation.JsonFormat
import io.circe.Decoder
import models.UserModel
import org.checkerframework.checker.units.qual.s
import play.*
import play.api.*
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import slick.jdbc.H2Profile.api.*
import play.api.mvc.*
import play.api.mvc.Results.Redirect
import schema.UserTable.UserRow
import slick.jdbc.JdbcProfile
import io.circe.generic.auto.*
import io.circe.syntax.*
import io.circe.*
import io.circe.parser.*

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.{ExecutionContext, Future}

class Auth @Inject() (
  protected val dbConfigProvider: DatabaseConfigProvider,
  cc: ControllerComponents
)(
  using ExecutionContext
) extends AbstractController(cc), HasDatabaseConfigProvider[JdbcProfile]:
  
  given Database = db.asInstanceOf[Database]
  
  def loginView = Action: (request: Request[AnyContent]) =>
    Ok(views.html.PageTemplate("LoginView"))
  
  def login = Action.async: (request: Request[AnyContent]) =>
    Auth.withForm[LoginForm](request): form =>
      UserModel().login(form).map: result =>
        val response = Ok(result.asJson.toString).as(JSON)
        result match
          case Right(userId) => response.withSession("userId" -> userId.toString)
          case _ => response
          
  def register = Action.async: (request: Request[AnyContent]) =>
    Auth.withForm[RegistrationForm](request): form =>
      UserModel().register(form).map: result =>
        val response = Ok(result.asJson.toString)
        result match
          case Right(userId) => response.withSession("userId" -> userId.toString)
          case _ => response
        
  def logout = Action: (request: Request[AnyContent]) =>
    Redirect("/").removingFromSession("userId")(using request)
    
  def current = Action.async: (request: Request[AnyContent]) =>
    Auth.currentUser(request).map: userRow =>
      Ok(userRow.map(u => User(u.id, u.username)).asJson.toString)

object Auth:

  def currentUser
    (request: Request[AnyContent])
    (using Database, ExecutionContext)
  : Future[Option[UserRow]] =
    
    request.session.get("userId").flatMap(_.toIntOption) match
      case Some(userId) => UserModel().getUserById(userId)
      case None => Future.successful(None)
      
  def withUser
    (request: Request[AnyContent])
    (f: UserRow => Result | Future[Result])
    (using Database, ExecutionContext)
  : Future[Result] =
    
    for
      user: Option[UserRow] <- currentUser(request)
      result = user match
        case None => Redirect("...")
        case Some(user) => f(user)
      result <- toFuture(result)
    yield result
    
  def withUserOrAnonymous
    (request: Request[AnyContent])
    (f: UserRow => Result | Future[Result])
    (using Request[AnyContent], Database, ExecutionContext)
  : Future[Result] =
    
    for
      user: Option[UserRow] <- currentUser(request)
      result = user match
        case None => Redirect("...")
        case Some(user) => f(user)
      result <- toFuture(result)
    yield result
    
  def withForm[X]
    (request: Request[AnyContent])
    (f: X => Future[Result])
    (using Decoder[X])
  : Future[Result] =
    
    val result = for
      text <- request.body.asJson
      json <- parse(text.toString).toOption
      result <- json.as[X].toOption
    yield result
    
    result match
      case Some(result) => f(result)
      case None => Future.successful(Redirect("/"))
    
  private def toFuture[X](x: X | Future[X]): Future[X] =
    if x.isInstanceOf[Future[AnyContent]] then x.asInstanceOf[Future[X]]
    else Future.successful(x.asInstanceOf[X])