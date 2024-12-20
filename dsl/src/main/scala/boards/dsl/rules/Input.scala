package boards.dsl.rules

import boards.imports.math.{*, given}
import boards.imports.games.{*, given}
import boards.math.region.Region.HasRegionI

sealed trait Input:
  val region: RegionI
  val from: RegionI
  
object Input:
  
  def click(region: HasRegionI): Input.Click =
    Input.Click(region.region)
    
  def drag(from: HasRegionI, to: HasRegionI): Input.Drag =
    Input.Drag(from.region, to.region)
    
  case class Click(region: RegionI) extends Input:
    val from = region
    override def toString = s"Click$region"
    
  case class Drag(from: RegionI, to: RegionI) extends Input:
    val region = from | to
    override def toString = s"Drag($from ~> $to)"