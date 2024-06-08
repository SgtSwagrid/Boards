package boards.components

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import com.raquo.laminar.api.L.{href, img, small, src, *, given}
import com.raquo.laminar.codecs.StringAsIsCodec
import com.raquo.laminar.nodes.{ReactiveElement, ReactiveSvgElement}

object InputField:
  
  def apply (
    name: String,
    icon: SvgElement,
    reference: Var[String],
    inputType: String = "text",
    focus: Boolean = false
  ): HtmlElement =
    
    label (
      className("input input-bordered flex items-center gap-2"),
      marginTop("5px"),
      marginBottom("5px"),
      icon,
      input(
        typ(inputType),
        className("grow"),
        placeholder(name),
        autoFocus(focus),
        value <-- reference,
        onInput.mapToValue --> reference
      )
    )