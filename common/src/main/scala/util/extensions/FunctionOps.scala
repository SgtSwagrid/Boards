package util.extensions

import scala.annotation.targetName
import scala.collection.mutable

object FunctionOps:
  
  extension [X, Y] (f: X => Y)
    def memoise: X => Y =
      val memo = mutable.HashMap[X, Y]()
      x => memo.getOrElseUpdate(x, f(x))
      
  extension [X] (f: X => Boolean)
    @targetName("not") def unary_! : X => Boolean = x => !f(x)
    @targetName("or") def || (g: X => Boolean): X => Boolean = x => f(x) || g(x)
    @targetName("and") def && (g: X => Boolean): X => Boolean = x => f(x) && g(x)
    @targetName("zor") def ^ (g: X => Boolean): X => Boolean = x => f(x) ^ g(x)