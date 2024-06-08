package boards.views

import boards.components.*
import org.scalajs.dom

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.nodes.ReactiveElement
import io.laminext.fetch.url

@JSExportTopLevel("IndexView")
object IndexView extends View:
  def content = div (
    Navbar(),
    Footer()
  )