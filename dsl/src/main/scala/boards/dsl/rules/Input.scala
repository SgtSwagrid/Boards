package boards.dsl.rules

import boards.math.vector.Region.RegionI
import io.circe.Codec
import boards.util.Codecs.given

sealed trait Input derives Codec.AsObject:
  val region: RegionI
  val from: RegionI
  
object Input:
  
  def click(region: RegionI): Input.Click =
    Input.Click(region)
    
  def drag(from: RegionI, to: RegionI): Input.Drag =
    Input.Drag(from, to)
    
  case class Click (region: RegionI) extends Input:
    val from = region
    override def toString = s"Click$region"
    
  case class Drag (from: RegionI, to: RegionI) extends Input:
    val region = from | to
    override def toString = s"Drag($from ~> $to)"