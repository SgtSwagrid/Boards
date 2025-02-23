package boards.math

import Surd.SurdTerm
import boards.math.Algebra.{*, given}
import boards.math.Conversions.{*, given}
import io.circe.Codec

/** An exact representation of a surd,
  * which is a number formed by the rationals and roots thereof.
  * Traditionally, surds allow for roots of any order. Here, only square roots are permitted.
  *
  * @param terms The terms which sum together to form this surd, each of which is a coefficient and a root.
  *
  * @author Alec Dorrington
  */
case class Surd (
  terms: Seq[SurdTerm]
) derives Codec.AsObject:
  
  /** A floating-point approximation of this value. */
  def approximate: Float =
    terms.map(_.approximate).sum
    
  def / [X: ToRational] (x: X): Surd = Surd:
    terms.map(t => t.copy(coefficient = t.coefficient / x.toRational))
  
  /** Simplify the surd by combining like terms. */
  private def simplify = Surd:
    terms.groupBy(_.root).map: (root, terms) =>
      val coefficient = terms.map(_.coefficient).reduce(_ + _)
      SurdTerm(coefficient, root)
    .filter(t => t.coefficient.numerator != 0 && t.root.numerator != 0)
    .toSeq
    .sortBy(_.root)
    
  override def toString = terms.mkString(" + ")

object Surd:
  
  /** A single term in a surd.
    * @param coefficient The coefficient of this term.
    * @param root The number inside the square root. Will be one for the rational term.
    */
  case class SurdTerm (
    coefficient: Rational,
    root: Rational,
  ) derives Codec.AsObject:
    
    /** A floating-point approximation of this term. */
    def approximate: Float =
      coefficient.approximate * Math.sqrt(root.approximate).toFloat
    
    override def toString =
      if root.numerator == 1 && root.denominator == 1 then coefficient.toString
      else s"$coefficient√$root"
  
  /** Create a [[Surd]] from an [[Int]]. */
  def integer (n: Int): Surd = Surd.rational(Rational.integer(n))
  
  /** Create a [[Surd]] from a [[Rational]]. */
  def rational (q: Rational): Surd = Surd(Seq(SurdTerm(q, Rational.one)))
  
  /** Create a [[Surd]] equal to the square root of an [[Int]]. */
  def sqrt (n: Int): Surd = Surd.sqrt(Rational.integer(n))
  
  /** Create a [[Surd]] equal to the square root of a [[Rational]]. */
  def sqrt (q: Rational): Surd = Surd(Seq(SurdTerm(Rational.one, q)))
  
  /** The [[Surd]] form of 0 = 0√1. */
  val zero = Surd(Seq.empty)
  
  /** The [[Surd]] form of 1 = 1√1. */
  val one = Surd.integer(1)
  
  given OrderedRing[Surd] with
    
    inline def sum (x: Surd, y: Surd) = Surd:
      x.terms ++ y.terms
    .simplify
      
    inline def additiveIdentity = Surd.zero
    
    inline def additiveInverse (x: Surd) = Surd:
      x.terms.map(t => t.copy(coefficient = -t.coefficient))
      
    inline def product (x: Surd, y: Surd) = Surd:
      for
        x <- x.terms
        y <- y.terms
      yield
        if x.root == y.root
        then SurdTerm(x.coefficient * y.coefficient * x.root, Rational.one)
        else SurdTerm(x.coefficient * y.coefficient, x.root * y.root)
    .simplify
    
    inline def multiplicativeIdentity = Surd.one
    
    //inline def multiplicativeInverse (x: Surd) = Surd:
    //  x.terms.map(t => SurdTerm(coefficient = t.coefficient.inverse, root = t.root.inverse))
      
    inline def compare (x: Surd, y: Surd) =
      x.toFloat.compare(y.toFloat)