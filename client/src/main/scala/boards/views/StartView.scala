package boards.views

import boards.Catalogue
import boards.protocol.GameProtocol.{CreateRoomRequest, CreateRoomResponse}
import boards.imports.laminar.{*, given}
import com.raquo.laminar.api.L.*
import boards.imports.circe.{*, given}

import scala.concurrent.ExecutionContext.Implicits.global

@JSExportTopLevel("StartView")
object StartView extends View:
  
  def content = div (
    Navbar(),
    div (
      className("grid gap-2 xs:grid-cols-1 md:grid-cols-2 lg:grid-cols-3 mx-auto"),
      position("absolute"),
      top("120px"), left("50px"), right("50px"),
      width("fit-content"),
      Catalogue.all.map: game =>
        div (
          onClick.flatMapTo:
            Fetch.post("/start", body=CreateRoomRequest(game.name))
              .decode[CreateRoomResponse].map(_.data)
          --> (response => Navigation.goto(s"/game/${response.roomId}")),
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