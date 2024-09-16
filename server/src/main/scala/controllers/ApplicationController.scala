package controllers

import boards.protocol.GameProtocol.{CreateRoomRequest, CreateRoomResponse}
import boards.imports.circe.{*, given}
import models.GameModel
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import play.api.mvc.*
import slick.jdbc.H2Profile.api.*
import slick.jdbc.JdbcProfile

import javax.inject.Inject
import scala.concurrent.ExecutionContext

class ApplicationController @Inject() (
  protected val dbConfigProvider: DatabaseConfigProvider,
  cc: ControllerComponents
) (
  using ExecutionContext
) extends AbstractController(cc), HasDatabaseConfigProvider[JdbcProfile]:
  
  given Database = db.asInstanceOf[Database]
  
  def indexView = Action: (request: Request[AnyContent]) =>
    Ok(views.html.PageTemplate("IndexView"))
    
  def startView = Action: (request: Request[AnyContent]) =>
    Ok(views.html.PageTemplate("StartView"))
  
  def browseView = Action: (request: Request[AnyContent]) =>
    Ok(views.html.PageTemplate("BrowseView"))
    
  def startRoom = Action.async: (request: Request[AnyContent]) =>
    AuthController.withUser(request): user =>
      AuthController.withForm[CreateRoomRequest](request): form =>
        GameModel().createRoom(user.id, form.gameId)
          .map(room => Ok(CreateRoomResponse(room.id).asJson.toString))