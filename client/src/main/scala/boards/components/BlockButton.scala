package boards.components

import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.nodes.ReactiveHtmlElement

import java.sql.Ref

object BlockButton:
  
  def apply
    (colour: String)
    (modifiers: Modifier[HtmlElement]*)
  : HtmlElement =
    
    button (
      Seq (
        className(s"btn btn-$colour"),
        width("100%"),
        marginBottom("10px"),
      ) ++ modifiers
      *
    )