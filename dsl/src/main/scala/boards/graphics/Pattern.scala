package boards.graphics

import boards.math.vector.Vec.VecI

/** Colour patterns for positionally selecting a background colour. */
object Pattern:
  
  /** A checkered pattern with alternating colours, similar to a chess board. */
  def Checkered (colours: Colour*) (pos: VecI): Colour =
    colours(pos.sum % colours.size)
    
  def CheckeredHex (c1: Colour, c2: Colour, c3: Colour) (pos: VecI): Colour =
    Seq(c1, c2, c3)((pos.x + 2 * pos.y) % 3)