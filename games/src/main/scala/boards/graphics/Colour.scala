package boards.graphics

case class Colour(hex: Int, name: String = ""):

  val r: Int = hex >> 16
  val g: Int = (hex >> 8) & 255
  val b: Int = hex & 255
  
  def lighten(amount: Int): Colour =
    Colour.rgb(Math.min(r + amount, 255), Math.min(g + amount, 255), Math.min(b + amount, 255))
  def darken(amount: Int): Colour = lighten(-amount)
  
  def apply(string: String): String =
    f"$name$string\u001B[0m"
    
  def + (colour: Colour): Colour =
    Colour.rgb(r + colour.r, g + colour.g, b + colour.b)
  def * (weight: Double): Colour =
    Colour.rgb((r * weight).toInt, (g * weight).toInt, (b * weight).toInt)
    
  def mix(colour: Colour, weight: Double = 0.5): Colour =
    (this * (1 - weight)) + (colour * weight)
    
  def hexString: String = s"#${hex.toHexString.reverse.padTo(6, "0").reverse.mkString}"
  override def toString: String = hexString
  
object Colour:
  
  def hex(hex: Int, name: String = ""): Colour =
    if hex < 0 || hex >= (1 << 24) then
      throw new IllegalArgumentException("HEX values must be in range [0, 2^24-1].")
    new Colour(hex, name)
  
  def rgb(r: Int, g: Int, b: Int, name: String = ""): Colour =
    if Seq(r, g, b).exists(c => c < 0 || c > 255) then
      throw new IllegalArgumentException("RGB values must be in range [0, 255].")
    Colour.hex((r << 16) + (g << 8) + b, name)
    
  def mix(colours: Colour*): Colour =
    colours.reduce(_ + _)
  
  val Black  : Colour = Colour.hex(0x000000, colour(0))
  val White  : Colour = Colour.hex(0xFFFFFF, colour(7))
  val Red    : Colour = Colour.hex(0xFF0000, colour(1))
  val Green  : Colour = Colour.hex(0x00FF00, colour(2))
  val Blue   : Colour = Colour.hex(0x0000FF, colour(4))
  val Yellow : Colour = Colour.hex(0xFFFF00, colour(3))
  val Magenta: Colour = Colour.hex(0xFF00FF, colour(5))
  val Cyan   : Colour = Colour.hex(0x00FFFF, colour(6))
  
  val ChessDark : Colour = Colour.hex(0xD18B47)
  val ChessLight: Colour = Colour.hex(0xFFCE9E)
  
  private def colour(n: Int): String = s"\u001B[3${n}m"