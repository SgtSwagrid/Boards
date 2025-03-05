package boards.dsl

import boards.math.Interval
import boards.math.vector.{Bounds, Box, Vec}
import boards.math.vector.Embedding.embed

object Test extends App:
  
  println:
    Interval.all & Interval.between(3, 5)
    
  println:
    Interval.between(3, 6) & Interval.all
  
  println:
    Box(8, 8).embed.logicalPositions.force
  
  println:
    Box(8, 8).embed.scale(2.0F).embeddedPositions.force
    
  println:
    Box(8, 8).embed.translate(-2.0F, 10.0F).embeddedPositions.force