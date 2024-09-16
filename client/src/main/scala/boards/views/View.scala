package boards.views

import boards.imports.laminar.{*, given}

abstract class View:
  
  def content: ReactiveElement.Base
  //def footer: ReactiveElement.Base = Footer()
  
  @JSExport("show")
  def show() =
    
    val root = document.getElementById("root")
    render(root, content)