package boards.views

import boards.components.{Tabs, *}
import boards.components.Tabs.Tab
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

@JSExportTopLevel("BrowseView")
object BrowseView extends View:
  
  def content = div (
    Navbar(),
    
    Footer()
  )