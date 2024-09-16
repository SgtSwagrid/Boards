package boards.components

import boards.imports.laminar.{*, given}
import com.raquo.laminar.api.L.*

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
        text,
      ),
      action,
    )