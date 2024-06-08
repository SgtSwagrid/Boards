package boards.components

import boards.protocol.UserProtocol.{LoginResponse, User}
import boards.util.Navigation
import org.scalajs.dom

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import com.raquo.laminar.api.L.{borderColor, button, href, img, small, src, *, given}
import com.raquo.laminar.codecs.StringAsIsCodec
import com.raquo.laminar.nodes.ReactiveElement
import com.raquo.laminar.api.features.unitArrows
import com.raquo.laminar.modifiers.EventListener
import io.laminext.syntax.core.*
import io.laminext.fetch.circe.*
import io.circe.*
import io.circe.syntax.*
import io.circe.parser.*
import io.circe.generic.auto.*

import scala.concurrent.ExecutionContext.Implicits.global

object Navbar:
  
  private val dataTip: HtmlProp[String, String] =
    htmlProp("data-tip", StringAsIsCodec)
  
  def apply() =
    
    val user = Fetch.post("/auth/current").decode[Option[User]].map(_.data)
    
    div (className("navbar bg-base-200"),
      height("70px"),
      paddingLeft("30px"),
      paddingRight("30px"),
      borderBottom("2px"),
      borderStyle("solid"),
      borderColor("#fbc531"),
      div (className("navbar-start"),
        ExpandingButton("/assets/images/ui/navbar/start.svg", "Start Game"):
          Navigation.goto("/start")
        ,
        ExpandingButton("/assets/images/ui/navbar/browse.svg", "Browse Games"):
          Navigation.goto("/browse")
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
        case Some(user) =>
          ExpandingButton("/assets/images/ui/navbar/logout.svg", "Logout"):
            onClick.flatMapTo(Fetch.post("/auth/logout").raw) --> Navigation.goto("/")
        case None =>
          ExpandingButton("/assets/images/ui/navbar/login.svg", "Login or Register"):
            Navigation.goto("/login", "next" -> dom.window.location.href)
      )
    )