package boards.views

import boards.Games
import boards.components.*
import boards.components.Tabs.Tab
import boards.protocol.GameProtocol.*
import boards.protocol.UserProtocol.*
import boards.util.Navigation
import boards.util.Navigation.gotoNext
import com.raquo.airstream.web.FetchOptions
import org.scalajs.dom

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.nodes.ReactiveElement
import com.raquo.laminar.api.features.unitArrows
import com.raquo.laminar.keys.StyleVendorPrefixes.o
import io.laminext.syntax.core.*
import io.laminext.fetch.circe.*
import io.circe.*
import io.circe.syntax.*
import io.circe.parser.*
import io.circe.generic.auto.*
import org.scalajs.dom.{KeyCode, KeyboardEvent}

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
      Games.all.map: game =>
        div (
          onClick.flatMapTo:
            Fetch.post("/start", body=CreateRoomRequest(game.id))
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
              p(className("text-lg font-bold"), game.getClass.getSimpleName.replace("$", "")),
              img(src("/assets/images/ui/navbar/start.svg"), className("h-5 w-5"), right("0px"))
            )
          )
        )
    ),
    Footer()
  )