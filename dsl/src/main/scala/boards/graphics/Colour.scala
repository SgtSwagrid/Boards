package boards.graphics

import boards.math.Number.clamp
import io.circe.Codec

/** A colour with R, G, B channels each with 8-bit precision.
 *
  * @param hex The 24-bit hex code for this colour.
  * @param name The human-readable name of this colour.
  */
case class Colour (hex: Int, alpha: Int = 255, name: String = "") derives Codec.AsObject:

  /** The red channel, ranging between 0 and 255. */
  val r: Int = hex >> 16
  
  /** The green channel, ranging between 0 and 255. */
  val g: Int = (hex >> 8) & 255
  
  /** The blue channel, ranging between 0 and 255. */
  val b: Int = hex & 255
  
  val a: Int = alpha
  
  /** Additively increase each channel by the given amount. */
  def lighten (amount: Int): Colour =
    Colour.rgb (
      clamp(r + amount, 0, 255),
      clamp(g + amount, 0, 255),
      clamp(b + amount, 0, 255),
    )
    
  /** Additively decrease each channel by the given amount. */
  def darken (amount: Int): Colour = lighten(-amount)
  
  /** Colour the text using this colour for console output, if applicable. */
  def apply (string: String): String =
    f"$name$string\u001B[0m"
    
  /** Additively combine both operands. */
  infix def + (colour: Colour): Colour =
    Colour.rgb(r + colour.r, g + colour.g, b + colour.b)
    
  /** Multiply each channel by a scalar. */
  infix def * (weight: Double): Colour =
    Colour.rgb((r * weight).toInt, (g * weight).toInt, (b * weight).toInt)
    
  /* Mix in some other colour to this one. */
  infix def mix (colour: Colour, weight: Double = 0.5): Colour =
    (this * (1 - weight)) + (colour * weight)
    
  def withAlpha (alpha: Int): Colour =
    if alpha < 0 || alpha > 255 then
      throw new IllegalArgumentException("Alpha values must be in range [0, 255].")
    copy(alpha = alpha)
  
  /** Convert this colour to a hex string that can be used in HTML documents. */
  def hexString: String = s"#${hex.toHexString.reverse.padTo(6, "0").reverse.mkString}"
  override def toString: String = hexString
  
object Colour:
  
  /** Construct a colour from a hex string. */
  def hex (hex: Int, name: String = ""): Colour =
    if hex < 0 || hex >= (1 << 24) then
      throw new IllegalArgumentException("HEX values must be in range [0, 2^24-1].")
    new Colour(hex, name=name)
  
  /** Construct a colour from RGB values each ranging from 0 to 255. */
  def rgb (r: Int, g: Int, b: Int, name: String = ""): Colour =
    if Seq(r, g, b).exists(c => c < 0 || c > 255) then
      throw new IllegalArgumentException("RGB values must be in range [0, 255].")
    Colour.hex((r << 16) + (g << 8) + b, name)
    
  /** Evenly mix all the given colours. */
  def mix(colours: Colour*): Colour =
    colours.reduce(_ + _) * (1.0D / colours.size.toDouble)
  
  /** 0x000000 */ val Black   = Colour.hex(0x000000, colourStr(0))
  /** 0xFFFFFF */ val White   = Colour.hex(0xFFFFFF, colourStr(7))
  /** 0xFF0000 */ val Red     = Colour.hex(0xFF0000, colourStr(1))
  /** 0x00FF00 */ val Green   = Colour.hex(0x00FF00, colourStr(2))
  /** 0x0000FF */ val Blue    = Colour.hex(0x0000FF, colourStr(4))
  /** 0xFFFF00 */ val Yellow  = Colour.hex(0xFFFF00, colourStr(3))
  /** 0xFF00FF */ val Magenta = Colour.hex(0xFF00FF, colourStr(5))
  /** 0x00FFFF */ val Cyan    = Colour.hex(0x00FFFF, colourStr(6))
  
  /** The colours of a chess board. */
  object Chess:
    /** 0xD18B47 */ val Dark      = Colour.hex(0xD18B47)
    /** 0xFFCE9E */ val Light     = Colour.hex(0xFFCE9E)
    /** 0xD28C45 */ val HexDark   = Colour.hex(0xD28C45)
    /** 0xE9AD70 */ val HexMedium = Colour.hex(0xE9AD70)
    /** 0xFFCF9F */ val HexLight  = Colour.hex(0xFFCF9F)
  
  /** The British Palette from [[https://flatuicolors.com/palette/gb]]. */
  object British:
    /** 0x00a8ff */ val ProtossPylon         = Colour.hex(0x00a8ff)
    /** 0x9c88ff */ val Periwinkle           = Colour.hex(0x9c88ff)
    /** 0xfbc531 */ val RiseNShine           = Colour.hex(0xfbc531)
    /** 0x4cd137 */ val DownloadProgress     = Colour.hex(0x4cd137)
    /** 0x487eb0 */ val Seabrook             = Colour.hex(0x487eb0)
    /** 0x0097e6 */ val VanadylBlue          = Colour.hex(0x0097e6)
    /** 0x8c7ae6 */ val MattPurple           = Colour.hex(0x8c7ae6)
    /** 0xe1b12c */ val NanohanachaGold      = Colour.hex(0xe1b12c)
    /** 0x44bd32 */ val SkirretGreen         = Colour.hex(0x44bd32)
    /** 0x40739e */ val Naval                = Colour.hex(0x40739e)
    /** 0xe84118 */ val NasturcianFlower     = Colour.hex(0xe84118)
    /** 0xf5f6fa */ val LynxWhite            = Colour.hex(0xf5f6fa)
    /** 0x7f8fa6 */ val BlueberrySoda        = Colour.hex(0x7f8fa6)
    /** 0x273c75 */ val MazarineBlue         = Colour.hex(0x273c75)
    /** 0x353b48 */ val BlueNights           = Colour.hex(0x353b48)
    /** 0xc23616 */ val HarleyDavidsonOrange = Colour.hex(0xc23616)
    /** 0xdcdde1 */ val HintOfPensive        = Colour.hex(0xdcdde1)
    /** 0x718093 */ val ChainGangGrey        = Colour.hex(0x718093)
    /** 0x192a56 */ val PicoVoid             = Colour.hex(0x192a56)
    /** 0x2f3640 */ val Electromagnetic      = Colour.hex(0x2f3640)
  
  /** The American Palette from [[https://flatuicolors.com/palette/us]]. */
  object American:
    /** 0x81ecec */ val FadedPoster   = Colour.hex(0x81ecec)
    /** 0x00cec9 */ val RobinsEggBlue = Colour.hex(0x00cec9)
    /** 0xffeaa7 */ val SourLemon     = Colour.hex(0xffeaa7)
  
  /** The French Palette from [[https://flatuicolors.com/palette/fr]]. */
  object French:
    /** 0x6a89cc */ val Livid         = Colour.hex(0x6a89cc)
    /** 0xb8e99 */  val ParadiseGreen = Colour.hex(0xb8e99)
    /** 0x78e08f */ val AuroraGreen   = Colour.hex(0x78e08f)
  
  /** The Swedish Palette from [[https://flatuicolors.com/palette/se]]. */
  object Swedish:
    /** 0x1e272e */ val BlackPearl = Colour.hex(0x1e272e)
  
  private def colourStr(n: Int): String = s"\u001B[3${n}m"