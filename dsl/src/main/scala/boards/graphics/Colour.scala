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
  
  object Chess:
    val Dark : Colour = Colour.hex(0xD18B47)
    val Light: Colour = Colour.hex(0xFFCE9E)
  
  object British:
    val ProtossPylon = Colour.hex(0x00a8ff)
    val Periwinkle = Colour.hex(0x9c88ff)
    val RiseNShine = Colour.hex(0xfbc531)
    val DownloadProgress = Colour.hex(0x4cd137)
    val Seabrook = Colour.hex(0x487eb0)
    val VanadylBlue = Colour.hex(0x0097e6)
    val MattPurple = Colour.hex(0x8c7ae6)
    val NanohanachaGold = Colour.hex(0xe1b12c)
    val SkirretGreen = Colour.hex(0x44bd32)
    val Naval = Colour.hex(0x40739e)
    val NasturcianFlower = Colour.hex(0xe84118)
    val LynxWhite = Colour.hex(0xf5f6fa)
    val BlueberrySoda = Colour.hex(0x7f8fa6)
    val MazarineBlue = Colour.hex(0x273c75)
    val BlueNights = Colour.hex(0x353b48)
    val HarleyDavidsonOrange = Colour.hex(0xc23616)
    val HintOfPensive = Colour.hex(0xdcdde1)
    val ChainGangGrey = Colour.hex(0x718093)
    val PicoVoid = Colour.hex(0x192a56)
    val Electromagnetic = Colour.hex(0x2f3640)
    
  object French:
    val Livid = Colour.hex(0x6a89cc)
    val ParadiseGreen = Colour.hex(0xb8e99)
    val AuroraGreen = Colour.hex(0x78e08f)
    
  object Swedish:
    val BlackPearl = Colour.hex(0x1e272e)
  
  private def colour(n: Int): String = s"\u001B[3${n}m"