package boards.math

import boards.math.Algebra.OrderedField
import boards.math.Interval.{IntervalF, IntervalI}
import io.circe.Codec

/** A rational (fractional) number, in its most reduced form.
  * Uses [[Int]]s for both the numerator and denominator,
  * meaning overflow is still possible.
  *
  * @param numerator The fraction's numerator.
  * @param denominator The fraction's denominator.
  *
  * @author Alec Dorrington
  */
case class Rational private (
  numerator: Int,
  denominator: Int,
) derives Codec.AsObject:
  
  /** The largest integer which is not larger than this value. */
  def previousInt: Int = numerator / denominator
  /** The smallest integer which is not smaller than this value. */
  def nextInt: Int = (numerator + denominator - 1) / denominator
  /** A floating-point approximation of this value. */
  def approximate: Float = numerator.toFloat / denominator.toFloat
  
  /** Cancel common factors from the numerator and denominator. */
  private def simplify: Rational =
    val gcd = Number.gcd(numerator, denominator)
    Rational(numerator / gcd, denominator / gcd)
    
  override def toString =
    if denominator == 1
    then numerator.toString
    else s"$numerator/$denominator"

object Rational:
  
  /** Create a [[Rational]] from an [[Int]]. */
  def integer (n: Int): Rational = Rational(n, 1)
  
  /** Create a [[Rational]] with an [[Int]] in the denominator. */
  def inverse (d: Int): Rational = Rational(1, d)
  
  /** Create a [[Rational]] from a given numerator and denominator.
    * The fraction is automatically simplified.
    */
  def fraction (n: Int, d: Int): Rational = Rational(n, d).simplify
  
  /** The [[Rational]] form of 0 = 0/1. */
  val zero = Rational.integer(0)
  
  /** The [[Rational]] form of 1 = 1/1. */
  val one = Rational.integer(1)
      
  given OrderedField[Rational] with
    
    inline def sum (x: Rational, y: Rational): Rational =
      val lcm = Number.lcm(x.denominator, y.denominator)
      Rational (
        x.numerator * (lcm / x.denominator) + y.numerator * (lcm / y.denominator),
        lcm
      )
      
    inline def additiveIdentity: Rational = Rational.zero
    
    inline def additiveInverse (q: Rational): Rational =
      Rational(-q.numerator, q.denominator)
      
    inline def product (x: Rational, y: Rational): Rational =
      Rational.fraction (
        x.numerator * y.numerator,
        x.denominator * y.denominator
      )
      
    inline def multiplicativeIdentity: Rational = Rational.one
      
    inline def multiplicativeInverse (q: Rational): Rational =
      Rational(q.denominator, q.numerator)
      
    inline def compare (x: Rational, y: Rational) =
      x.numerator * y.denominator - y.numerator * x.denominator