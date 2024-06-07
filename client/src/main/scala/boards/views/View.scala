package boards.views

import org.scalajs.dom
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.nodes.ReactiveElement

abstract class View:
  
  def content: ReactiveElement.Base
  
  @JSExport("show")
  def show() =
    val root = dom.document.getElementById("root")
    render(root, content)