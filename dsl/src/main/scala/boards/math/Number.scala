package boards.math

import scala.annotation.tailrec
import scala.math.Ordered.orderingToOrdered

object Number:
  
  /**
   * Compute the greatest common divisor of all given integers,
   * using Euclid's algorithm.
   *
   * @see <a href="https://en.wikipedia.org/wiki/Greatest_common_divisor">
   *
   * @throws UnsupportedOperationException if the list of numbers is empty.
   */
  def gcd (x: Int*): Int =
    
    @tailrec
    def gcd2 (a: Int, b: Int): Int =
      if a == b || b == 0 then a
      else gcd2(b, a % b)
      
    x.filter(_ != 0).reduce(gcd2)
  
  /**
   * Compute the least common multiple of all given integers,
   * using Euclid's method.
   *
   * @see <a href="https://en.wikipedia.org/wiki/Least_common_multiple">
   *
   * @throws UnsupportedOperationException if the list of numbers is empty.
   */
  def lcm (x: Int*): Int =
    
    def lcm2 (a: Int, b: Int) =
      if a == 0 && b == 0 then 0
      else a.abs * (b.abs / gcd(a, b))
      
    x.reduce(lcm2)
    
  def clamp [X: Ordering] (x: X, min: X, max: X): X =
    if x < min then min
    else if x > max then max
    else x