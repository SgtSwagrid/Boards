package boards.components

import boards.protocol.UserProtocol.{LoginResponse, User}
import boards.util.Navigation
import org.scalajs.dom

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import com.raquo.laminar.api.L.{borderColor, button, href, img, small, src, *, given}
import com.raquo.laminar.codecs.StringAsIsCodec
import com.raquo.laminar.nodes.ReactiveElement
import com.raquo.laminar.api.features.unitArrows
import io.laminext.syntax.core.*
import io.laminext.fetch.circe.*
import io.circe.*
import io.circe.syntax.*
import io.circe.parser.*
import io.circe.generic.auto.*

import scala.concurrent.ExecutionContext.Implicits.global

object NavigationBar:
  
  private val dataTip: HtmlProp[String, String] =
    htmlProp("data-tip", StringAsIsCodec)
  
  def apply() =
    
    val user = Fetch.post("/auth/current").decode[Option[User]].map(_.data)
    
    div (className("navbar bg-base-200"),
      height("70px"),
      paddingLeft("30px"),
      paddingRight("30px"),
      borderBottom("1px"),
      borderStyle("solid"),
      borderColor("#fbc531"),
      div (className("navbar-start"),
        button (
          className("btn btn-ghost btn-circle"),
          img (
            src("/assets/images/ui/navbar/start.svg"),
            className("h-5 w-5")
            )
          ),
          button (
            className("btn btn-ghost btn-circle"),
            img (
              src("/assets/images/ui/navbar/browse.svg"),
              className("h-5 w-5"),
            )
          )
        ),
        div (className("navbar-center"),
          a (
            className("btn btn-ghost text-xl"),
            href("/"),
            "Boards"
          )
        ),
        div (className("navbar-end"),
          
          child.maybe <-- user.optionMap: user =>
            a (
              className("btn btn-ghost text-sm"),
              user.username,
              onClick --> Navigation.goto(s"/user/${user.username}")
            )
          ,
          
          child <-- user.map:
            case Some(user) => button (
              className("btn btn-ghost btn-circle"),
              img (
                src("/assets/images/ui/navbar/logout.svg"),
                className("h-5 w-5")
              ),
              onClick.mapToEvent.flatMapTo(Fetch.post("/auth/logout").raw) --> Navigation.gotoNext()
            )
            case None => button (
              className("btn btn-ghost btn-circle"),
              img (
                src("/assets/images/ui/navbar/login.svg"),
                className("h-5 w-5")
              ),
              onClick --> Navigation.goto(s"/login", "next" -> dom.window.location.href)
            )
        )
      )