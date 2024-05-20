package controllers

import play.api.*
import play.api.mvc.*

import javax.inject.Inject
import scala.concurrent.ExecutionContext

class Application  @Inject()
  (cc: ControllerComponents)
  (using ExecutionContext)
extends AbstractController(cc):
  
  def index = Action: (request: Request[AnyContent]) =>
    Ok("Hello, World! 12345")