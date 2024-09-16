package boards.graphics

import boards.imports.math.{*, given}

object Pattern:
  
  def Checkered(colours: Colour*)(pos: Vec[Int]): Colour =
    colours(pos.sum % colours.size)