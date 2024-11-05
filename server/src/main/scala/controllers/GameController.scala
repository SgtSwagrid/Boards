package controllers

import boards.graphics.Scene
import boards.protocol.GameProtocol.GameRequest
import play.api.*
import play.api.mvc.*
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import play.api.libs.streams.ActorFlow
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.scaladsl.*
import slick.jdbc.JdbcProfile
import slick.jdbc.H2Profile.api.*
import boards.imports.circe.{*, given}
import boards.protocol.UserProtocol.User
import models.GameModel

import scala.concurrent.{ExecutionContext, Future}
import javax.inject.{Inject, Singleton}

@Singleton
class GameController @Inject() (
  protected val dbConfigProvider: DatabaseConfigProvider,
  cc: ControllerComponents,
) (using ExecutionContext, ActorSystem)
  extends AbstractController(cc), HasDatabaseConfigProvider[JdbcProfile]:
  
  given Database = db.asInstanceOf[Database]
  
  val system = summon[ActorSystem].actorOf(SystemActor.props, "system")
  
  def gameView(id: String, join: Boolean) =
    if join then
      Action.async: (request: Request[AnyContent]) =>
        AuthController.withUser(request): _ =>
          Ok(views.html.PageTemplate("GameView"))
    else
      Action: (request: Request[AnyContent]) =>
        Ok(views.html.PageTemplate("GameView"))
    
  def gameSocket(id: String) = WebSocket.acceptOrResult[String, String]: request =>
    AuthController.currentUser(request)
      .zip(GameModel().getRoomById(id))
      .map: (user, room) =>
        room match
          case Some(room) => Right (
            ActorFlow
              .actorRef[String, Scene] { out => SessionActor.props (
                out, system, room.id, user.map(user => User(user.id, user.username))
              )}
              .map(_.asJson.toString)
          )
          case None => Left(NotFound)
        