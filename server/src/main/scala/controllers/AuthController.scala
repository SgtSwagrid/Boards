package controllers

import boards.protocol.UserProtocol.{LoginForm, RegistrationForm, User}
import io.circe.Decoder
import io.circe.parser.{decode, parse}
import io.circe.syntax.{EncoderOps, KeyOps}
import models.UserModel
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import play.api.mvc.*
import play.api.mvc.Results.Redirect
import schema.UserTable.UserRow
import slick.jdbc.H2Profile.api.*
import slick.jdbc.JdbcProfile

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class AuthController @Inject() (
  protected val dbConfigProvider: DatabaseConfigProvider,
  cc: ControllerComponents
)(
  using ExecutionContext
) extends AbstractController(cc), HasDatabaseConfigProvider[JdbcProfile]:
  
  given Database = db.asInstanceOf[Database]
  
  def loginView = Action: (request: Request[AnyContent]) =>
    Ok(views.html.PageTemplate("LoginView"))
  
  def login = Action.async: (request: Request[AnyContent]) =>
    AuthController.withForm[LoginForm](request): form =>
      UserModel().login(form).map: result =>
        val response = Ok(result.asJson.toString).as(JSON)
        result match
          case Right(userId) => response.withSession("userId" -> userId.toString)
          case _ => response
          
  def register = Action.async: (request: Request[AnyContent]) =>
    AuthController.withForm[RegistrationForm](request): form =>
      UserModel().register(form).map: result =>
        val response = Ok(result.asJson.toString)
        result match
          case Right(userId) => response.withSession("userId" -> userId.toString)
          case _ => response
        
  def logout = Action: (request: Request[AnyContent]) =>
    Redirect("/").removingFromSession("userId")(using request)
    
  def current = Action.async: (request: Request[AnyContent]) =>
    AuthController.currentUser(request).map: userRow =>
      Ok(userRow.map(_.toUser).asJson.toString)

object AuthController:

  def currentUser
    (request: RequestHeader)
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
        case None => Redirect(s"/login?next=${request.uri}")
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
    
  def withForm [X]
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