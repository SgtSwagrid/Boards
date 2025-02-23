package boards.views

import boards.components.{Navbar, Footer}
import boards.protocol.UserProtocol.User
import com.raquo.laminar.api.L.{*, given}

import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("IndexView")
object IndexView extends View:
  def content (user: Option[User]) = div (
    Navbar(user),
    Footer()
  )