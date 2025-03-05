package boards.math.ops

import boards.math.algebra.Algebra.{*, given}
import boards.math.algebra.Unbounded
import boards.math.algebra.Unbounded.{Finite, PositiveInfinity, NegativeInfinity}
import boards.math.algebra.Bijection.AffineBijection
import boards.math.vector.Vec
import boards.math.vector.Vec.UVec

object TransformOps:
  
  /*trait ScalarFunctor [X: Numeric, +This[Y] <: ScalarFunctor[Y, This]]:
    
    def map [Y: Numeric] (f: X => Y): This[Y]
    
  trait VecFunctor [X: Numeric, +This[Y] <: VecFunctor[Y, This]]:
    
    def map [Y: Numeric] (f: Vec[X] => Vec[Y]): This[Y]
    
    def mapComponents [Y: Numeric] (f: X => Y): This[Y] =
      map(_.map(f))*/

  trait UScalarFunctor [X: Numeric, +This[Y] <: UScalarFunctor[Y, This]]:
    
    def map [Y: Numeric] (f: Unbounded[X] => Unbounded[Y]): This[Y]
    
    def mapFinite [Y: Numeric] (f: X => Y): This[Y] = map:
      case Finite(x) => Finite(f(x))
      case PositiveInfinity => PositiveInfinity
      case NegativeInfinity => NegativeInfinity
  
  trait AffineFunctor [X: Numeric, +This[Y] <: AffineFunctor[Y, This]]:
    
    def mapAffine [Y: Numeric] (f: AffineBijection[X, Y]): This[Y]
  
  trait UVecFunctor [X: Numeric, +This[Y] <: UVecFunctor[Y, This]] extends AffineFunctor[X, This]:
    
    def map [Y: Numeric] (f: UVec[X] => UVec[Y]): This[Y]
    
    def mapComponents [Y: Numeric] (f: Unbounded[X] => Unbounded[Y]): This[Y] =
      map(_.map(f))
    
    def mapFinite [Y: Numeric] (f: X => Y): This[Y] = mapComponents:
      case Finite(x) => Finite(f(x))
      case PositiveInfinity => PositiveInfinity
      case NegativeInfinity => NegativeInfinity
      
    def mapAffine [Y: Numeric] (f: AffineBijection[X, Y]): This[Y] =
      map(f)