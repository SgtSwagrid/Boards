package boards.util.extensions

import boards.graphics.Colour
import com.raquo.laminar.api.L.*

object ColourOps:

  extension (colour: Colour)
    
    def textColour = color(colour.hexString)
    def backgroundColour = backgroundColor(colour.hexString)