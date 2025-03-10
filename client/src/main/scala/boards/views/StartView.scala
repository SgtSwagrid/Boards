package boards.views

import boards.GameCatalogue
import boards.components.{Footer, Navbar}
import boards.dsl.meta.Game
import boards.protocol.GameProtocol.{CreateRoomRequest, CreateRoomResponse}
import com.raquo.laminar.api.L.*
import boards.protocol.UserProtocol.User
import io.laminext.fetch.circe.Fetch
import org.scalajs.dom.document

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.annotation.JSExportTopLevel
import io.laminext.fetch.circe.jsonRequestBody
import io.laminext.fetch.circe.fetchEventStreamBuilderSyntaxCirce
import boards.util.Navigation.*

import io.laminext.syntax.core.*

@JSExportTopLevel("StartView")
object StartView extends View:
    
  def createRequest (game: String) =
    Fetch.post("/start", body=CreateRoomRequest(game))
      .decode[CreateRoomResponse].map(_.data)
  
  def content (user: Option[User]) = div (
    Navbar(user),
    div (
      className("grid gap-2 xs:grid-cols-1 md:grid-cols-2 lg:grid-cols-3 mx-auto"),
      position("absolute"),
      top("120px"), left("50px"), right("50px"),
      width("fit-content"),
      GameCatalogue.all.map: game =>
        div (
          onClick.flatMapTo(createRequest(game.name))
            --> (response => goto(s"/game/${response.roomId}")),
          className("card bg-neutral shadow-xl hover:brightness-125 active:brightness-150"),
          width("300px"),
          margin("25px"),
          figure (
            div (
              width("100%"),
              height("200px"),
              backgroundColor("grey")
            )
          ),
          div (
            className("card-body"),
            div (
              className("flex justify-between"),
              p(className("text-lg font-bold"), game.name),
              img(src("/assets/images/ui/navbar/start.svg"), className("h-5 w-5"), right("0px"))
            )
          )
        )
    ),
    Footer()
  )