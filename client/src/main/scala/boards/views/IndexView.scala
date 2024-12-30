package boards.views

import boards.imports.laminar.{*, given}
import boards.protocol.UserProtocol.User

@JSExportTopLevel("IndexView")
object IndexView extends View:
  def content(user: Option[User]) = div (
    Navbar(user),
    Footer()
  )