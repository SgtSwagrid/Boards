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
      className(s"btn btn-$colour no-animation"),
      width("100%"),
      marginBottom("10px"),
      modifiers,
    )
    
  def dropdown
    (colour: String)
    (modifiers: Modifier[HtmlElement]*)
    (contents: Seq[HtmlElement])
  : HtmlElement =
    
    div (
      className("dropdown dropdown-right dropdown-end"),
      width("100%"),
      marginBottom("10px"),
      div (
        className(s"btn btn-$colour no-animation"),
        width("100%"),
        tabIndex(0),
        role("button"),
        modifiers,
      ),
      ul (
        tabIndex(0),
        className("dropdown-content menu box"),
        backgroundColor("#353b48"),
        opacity("98%"),
        width("210px"),
        marginLeft("25px"),
        zIndex(10),
        contents.map(item => li(item)),
      ),
    )