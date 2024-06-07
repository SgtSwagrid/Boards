package boards.components

import org.scalajs.dom

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import com.raquo.laminar.api.L.{href, img, small, src, *, given}
import com.raquo.laminar.codecs.StringAsIsCodec
import com.raquo.laminar.nodes.{ReactiveElement, ReactiveHtmlElement}

object Tabs:
  
  def apply(tabs: Tab*): ReactiveElement.Base =
    val active = Var[Int](0)
    div (
      div (
        role("tablist"),
        className("tabs tabs-boxed"),
        tabs.zipWithIndex.map: (tab, i) =>
          a (
            role("tab"),
            className <-- active.signal.map: active =>
              if active == i then "tab tab-active" else "tab"
            ,
            onClick.mapTo(i) --> active,
            tab.header
          ),
      ),
      child <-- active.signal.map: i =>
        div (
          paddingTop("25px"),
          tabs(i).content
        )
    )
  
  case class Tab (
    header: ReactiveHtmlElement[?],
    content: ReactiveHtmlElement[?]
  )