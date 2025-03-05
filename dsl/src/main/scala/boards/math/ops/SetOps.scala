package boards.math.ops

object SetOps:
  
  trait Union [This <: Union[This]]:
    
    def union (that: This): This
    
    infix def | (that: This): This = union(that)
    
  trait Intersection [This <: Intersection[This]]:
    
    def intersect (that: This): This
    
    infix def & (that: This): This = intersect(that)
    
  trait SetMinus [This <: Intersection[This]]:
  
    def difference (that: This): This
    
    infix def | (that: This): This = difference(that)
    
  trait SymmetricDifference [This <: SymmetricDifference[This]]:
    
    def symmDiff (that: This): This
    
    def ^ (that: This): This = symmDiff(that)