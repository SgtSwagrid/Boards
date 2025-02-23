package boards.graphics

import boards.math.region.Vec

/** Colour patterns for positionally selecting a background colour. */
object Pattern:
  
  /** A checkered pattern with alternating colours, similar to a chess board. */
  def Checkered (colours: Colour*) (pos: Vec[Int]): Colour =
    colours(pos.sum % colours.size)