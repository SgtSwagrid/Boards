package boards.graphics

import util.math.Vec

object Pattern:
  
  def Checkered(colours: Colour*)(pos: Vec[Int]): Colour =
    colours(pos.sum % colours.size)