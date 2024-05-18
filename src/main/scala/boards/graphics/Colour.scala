package boards.graphics

case class Colour private (hex: Int, name: String = ""):

  val r: Int = hex >> 16
  val g: Int = (hex >> 8) & 255
  val b: Int = hex & 255
  
  def apply(string: String): String =
    f"$name$string\u001B[0m"
  
object Colour:
  
  def hex(hex: Int, name: String = ""): Colour =
    if hex < 0 || hex >= (1 << 24) then
      throw new IllegalArgumentException("HEX values must be in range [0, 2^24-1].")
    new Colour(hex, name)
  
  def rgb(r: Int, g: Int, b: Int, name: String = ""): Colour =
    if Seq(r, g, b).exists(c => c < 0 || c > 255) then
      throw new IllegalArgumentException("RGB values must be in range [0, 255].")
    Colour.hex((r << 16) + (g << 8) + b, name)
  
  val Black  : Colour = Colour.hex(0x000000, colour(0))
  val White  : Colour = Colour.hex(0xFFFFFF, colour(7))
  val Red    : Colour = Colour.hex(0xFF0000, colour(1))
  val Green  : Colour = Colour.hex(0x00FF00, colour(2))
  val Blue   : Colour = Colour.hex(0x0000FF, colour(4))
  val Yellow : Colour = Colour.hex(0xFFFF00, colour(3))
  val Magenta: Colour = Colour.hex(0xFF00FF, colour(5))
  val Cyan   : Colour = Colour.hex(0x00FFFF, colour(6))
  
  private def colour(n: Int): String = s"\u001B[3${n}m"