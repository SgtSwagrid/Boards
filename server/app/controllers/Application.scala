package controllers

import play.api.*
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import play.api.mvc.*
import slick.jdbc.JdbcProfile

import io.circe.generic.auto.*
import io.circe.syntax.*

import javax.inject.Inject
import scala.concurrent.ExecutionContext

class Application @Inject() (
  protected val dbConfigProvider: DatabaseConfigProvider,
  cc: ControllerComponents
) (
  using ExecutionContext
) extends AbstractController(cc), HasDatabaseConfigProvider[JdbcProfile]:
  
  def index = Action: (request: Request[AnyContent]) =>
    Ok(views.html.PageTemplate("IndexView"))