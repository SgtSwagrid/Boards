package boards.views

import boards.imports.laminar.{*, given}

@JSExportTopLevel("BrowseView")
object BrowseView extends View:
  
  def content = div (
    Navbar(),
    Footer()
  )