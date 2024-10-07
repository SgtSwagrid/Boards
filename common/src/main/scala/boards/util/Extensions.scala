package boards.util

object Extensions:
  
  extension [X] (sx: Seq[X])
    
    def intersperse[Y](y: Y): Seq[X | Y] =
      sx.flatMap(x => Seq(y, x)).drop(1)
      
    def interweave[Y](sy: Seq[Y]): Seq[X | Y] =
      sx.map(Option.apply).zipAll(sy.map(Option.apply), None, None).flatMap(_ ++ _)