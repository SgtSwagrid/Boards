package boards.components

import boards.protocol.UserProtocol.{LoginResponse, User}
import boards.imports.circe.{*, given}
import boards.imports.laminar.{*, given}
import com.raquo.laminar.api.L.*

object Navbar:
  
  private val dataTip: HtmlProp[String, String] =
    HtmlProp("data-tip", StringAsIsCodec)
    
  val navbarHeight: Int = 80
  
  def apply(user: Option[User]) =
    
    div (
      className("navbar bg-base-200"),
      position("fixed"),
      width("100%"),
      height(s"${navbarHeight}px"),
      paddingLeft("30px"),
      paddingRight("30px"),
      borderBottom("2px"),
      borderStyle("solid"),
      borderColor("#fbc531"),
      zIndex("100"),
      div (className("navbar-start"),
        ExpandingButton("/assets/images/ui/navbar/start.svg", "Start Game"):
          if user.isDefined
          then Navigation.goto("/start")
          else Navigation.goto("/login", "next" -> "/start")
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
      div (
        className("navbar-end"),
      
        user.map: user =>
          a (
            className("btn btn-ghost text-sm"),
            user.username,
            onClick --> Navigation.goto(s"/user/${user.username}")
          )
        ,
        user match
          case Some(user) =>
            ExpandingButton("/assets/images/ui/navbar/logout.svg", "Logout"):
              onClick.flatMapTo(Fetch.post("/auth/logout").raw) --> Navigation.goto("/")
          case None =>
            ExpandingButton("/assets/images/ui/navbar/login.svg", "Login or Register"):
              Navigation.goto("/login", "next" -> window.location.href)
      )
    )