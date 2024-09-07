package boards.views

import boards.components.Footer
import boards.util.Navigation
import org.scalajs.dom

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.nodes.ReactiveElement
import com.raquo.laminar.api.features.unitArrows
import io.laminext.websocket.*

abstract class View:
  
  def content: ReactiveElement.Base
  //def footer: ReactiveElement.Base = Footer()
  
  @JSExport("show")
  def show() =
    
    val root = dom.document.getElementById("root")
    render(root, content)