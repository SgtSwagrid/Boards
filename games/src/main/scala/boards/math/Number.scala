package boards.math

import scala.annotation.tailrec

object Number:
  
  def gcd(x: Int*): Int =
    
    @tailrec
    def gcd2(a: Int, b: Int): Int =
      if a == b || b == 0 then a
      else gcd2(b, a % b)
      
    x.reduce(gcd2)
    
  def lcm(x: Int*): Int =
    
    def lcm2(a: Int, b: Int) =
      if a == 0 && b == 0 then 0
      else a.abs * (b.abs / gcd(a, b))
      
    x.reduce(lcm2)