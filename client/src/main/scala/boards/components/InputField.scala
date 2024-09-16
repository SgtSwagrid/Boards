package boards.components

import boards.imports.laminar.{*, given}

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