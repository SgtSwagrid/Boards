package boards.components

import boards.components.Footer.aside
import org.scalajs.dom
import com.raquo.laminar.api.L.{a, br, button, href, img, small, src, *, given}
import com.raquo.laminar.codecs.StringAsIsCodec
import com.raquo.laminar.nodes.ReactiveElement
import com.raquo.laminar.api.features.unitArrows
import com.raquo.laminar.modifiers.EventListener
import com.raquo.laminar.tags.HtmlTag
import org.scalajs.dom.MouseEvent

object ExpandingButton:
  
  def apply(icon: String, text: String)(action: => Unit): HtmlElement =
    apply(icon, text):
      onClick --> action
  
  def apply(icon: String, text: String)(action: Modifier.Base) =
    button (
      className("group btn btn-ghost [&:not(:hover)]:btn-circle"),
      img(src(icon), className("h-5 w-5")),
      p (
        className("invisible w-0 group-hover:visible group-hover:w-24"),
        text
      ),
      action
    )