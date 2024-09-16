package boards.components

import boards.imports.laminar.{*, given}

object Tabs:
  
  def apply(tabs: Tab*): HtmlElement =
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
    header: HtmlElement,
    content: HtmlElement
  )