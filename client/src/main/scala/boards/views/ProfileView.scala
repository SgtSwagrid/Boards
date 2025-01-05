package boards.views

import boards.components.{Footer, Navbar}
import boards.protocol.UserProtocol.User
import com.raquo.laminar.api.L.div
import org.scalajs.dom.document
import com.raquo.laminar.api.L.{*, given}
import io.laminext.fetch.Fetch
import boards.imports.circe.{*, given}
import io.laminext.fetch.circe.fetchEventStreamBuilderSyntaxCirce
import scala.concurrent.ExecutionContext.Implicits.global

import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("ProfileView")
object ProfileView extends View:
  
  private lazy val username: String =
    document.documentURI.split("/").dropWhile(_ != "user").drop(1).head
    
  private lazy val user: Signal[Option[User]] =
    Fetch.get(s"/user/${username}/details").decode[Option[User]].map(_.data)
      .startWith(None)
  
  def content(validatedUser: Option[User]) = div (
    Navbar(validatedUser),
    div (
      position("absolute"),
      top(s"${Navbar.navbarHeight + 40}px"),
      left("100px"),
      child <-- user.map:
        case Some(user) => div (
          h1(user.username),
          p(s"Joined on ${user.joined}"),
        )
        case None => div()
    ),
    Footer(),
  )
