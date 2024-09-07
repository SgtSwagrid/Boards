package boards.graphics

enum Shape:
  case Rectangle(width: Int = 1, height: Int = 1)
  case Hexagon
  case EquilateralTriangle(orientation: Int)