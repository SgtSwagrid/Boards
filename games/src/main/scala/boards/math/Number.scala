package boards.math

import boards.math.Vec.VecI
import scala.annotation.tailrec

object Number:
  
  /**
   * Compute the greatest common divisor of all given integers,
   * using Euclid's algorithm.
   *
   * @see <a href="https://en.wikipedia.org/wiki/Greatest_common_divisor">
   *
   * @throws UnsupportedOperationException if the list of numbers is empty.
   */
  def gcd(x: Int*): Int =
    
    @tailrec
    def gcd2(a: Int, b: Int): Int =
      if a == b || b == 0 then a
      else gcd2(b, a % b)
      
    x.reduce(gcd2)
  
  /**
   * Compute the least common multiple of all given integers,
   * using Euclid's method.
   *
   * @see <a href="https://en.wikipedia.org/wiki/Least_common_multiple">
   *
   * @throws UnsupportedOperationException if the list of numbers is empty.
   */
  def lcm(x: Int*): Int =
    
    def lcm2(a: Int, b: Int) =
      if a == 0 && b == 0 then 0
      else a.abs * (b.abs / gcd(a, b))
      
    x.reduce(lcm2)