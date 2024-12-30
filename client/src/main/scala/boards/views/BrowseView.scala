package boards.views

import boards.components.{Footer, Navbar}
import boards.protocol.UserProtocol.User
import com.raquo.laminar.api.L.*

import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("BrowseView")
object BrowseView extends View:
  
  def content(user: Option[User]) = div (
    Navbar(user),
    div (
      position("absolute"),
      top(s"${Navbar.navbarHeight + 40}px"),
      left("100px"),
      p("TODO: Show list of all existing games here..."),
    ),
    Footer()
  )