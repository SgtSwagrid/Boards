package boards.views

import boards.imports.laminar.{*, given}

@JSExportTopLevel("IndexView")
object IndexView extends View:
  def content = div (
    Navbar(),
    Footer()
  )