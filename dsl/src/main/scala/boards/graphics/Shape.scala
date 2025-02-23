package boards.graphics

import Shape.*
import io.circe.Codec

enum Shape derives Codec.AsObject:
  case Rectangle
  case Triangle (orientation: TriangleOrientation)
  case Hexagon (orientation: Orientation)

object Shape:
  
  enum Orientation derives Codec.AsObject:
    case Vertical, Horizontal
  
  enum TriangleOrientation derives Codec.AsObject:
    case Up, Down, Left, Right
    def isVertical: Boolean = this match
      case Up | Down => true
      case Left | Right => false
    def isHorizontal: Boolean = !isVertical