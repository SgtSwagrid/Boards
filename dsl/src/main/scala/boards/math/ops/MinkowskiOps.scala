package boards.math.ops

object MinkowskiOps:
  
  trait Sum [This <: Sum[This]]:
    
    def minkowskiSum (that: This): This
    
    infix def + (that: This): This = minkowskiSum(that)
    
  trait Negate [This <: Negate[This]]:
    
    def negate: This
    
    def unary_- : This = negate
    
  trait Difference [This <: Difference[This]] extends Sum[This], Negate[This]:
    
    infix def - (that: This): This = this + (-that)