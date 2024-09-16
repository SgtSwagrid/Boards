package boards.components

import boards.protocol.UserProtocol.{LoginResponse, User}
import boards.imports.circe.{*, given}
import boards.imports.laminar.{*, given}
import com.raquo.laminar.api.L.*

object Navbar:
  
  private val dataTip: HtmlProp[String, String] =
    HtmlProp("data-tip", StringAsIsCodec)
  
  def apply() =
    
    import scala.concurrent.ExecutionContext.Implicits.global
    val user = Fetch.post("/auth/current").decode[Option[User]].map(_.data)
    
    div (
      className("navbar bg-base-200"),
      position("fixed"),
      width("100%"),
      height("70px"),
      paddingLeft("30px"),
      paddingRight("30px"),
      borderBottom("2px"),
      borderStyle("solid"),
      borderColor("#fbc531"),
      zIndex("100"),
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
      
      child.maybe <-- user.map: u =>
        u.map: user =>
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
            Navigation.goto("/login", "next" -> window.location.href)
      )
    )