package boards.math.region

import boards.math.Algebra.{*, given}
import boards.math.Bijection.<=>
import Vec.HasVec
import boards.math.WithInfinity.{ExtendedDouble, ExtendedFloat, ExtendedInt, ExtendedLong}
import boards.math.{Interval, WithInfinity}

enum BoundingBox [@specialized X: Ring as R: Ordering as O]:
  
  case NonEmpty [Y: Ring: Ordering] private[math] (
    start: Vec[Y],
    end: Vec[Y],
  ) extends BoundingBox[Y]
  
  case Empty [Y: Ring: Ordering] () extends BoundingBox[Y]
  
  val size: Vec[X] = this match
    case NonEmpty(start, end) => end - start + Vec.one(Math.max(start.dim, end.dim))
    case Empty() => Vec.empty
  
  export size.{dim, product => area}
  
  def inBounds (v: HasVec[X]): Boolean = this match
    case NonEmpty(start, end) => start <= v.position && v.position <= end
    case Empty() => false
  
  def inBounds (v: X*): Boolean = inBounds(Vec(v))
    
  def inBounds [Y: Ring: Ordering] (v: HasVec[Y]) (using X =:= WithInfinity[Y]): Boolean =
    inBounds(v.position.asUnbounded.asInstanceOf[Vec[X]])
    
  def inBounds [Y: Ring: Ordering] (v: Y*) (using X =:= WithInfinity[Y]): Boolean =
    inBounds(Vec(v*))
  
  def | (that: BoundingBox[X]): BoundingBox[X] = (this, that) match
    case (NonEmpty(s1, e1), NonEmpty(s2, e2)) =>
      BoundingBox(s1.zip(s2)(O.min), e1.zip(e2)(O.max))
    case (left: BoundingBox.NonEmpty[X], BoundingBox.Empty()) => left
    case (Empty(), right: NonEmpty[X]) => right
    case _ => BoundingBox.empty
  
  def & (that: BoundingBox[X]): BoundingBox[X] = (this, that) match
    case (NonEmpty(s1, e1), NonEmpty(s2, e2)) =>
      BoundingBox(s1.zip(s2)(O.max), e1.zip(e2)(O.min))
    case _ => BoundingBox.empty
  
  def unary_- : BoundingBox[X] = this match
    case NonEmpty(start, end) => BoundingBox(-end, -start)
    case _ => BoundingBox.empty
  
  def + (that: BoundingBox[X]): BoundingBox[X] = (this, that) match
    case (NonEmpty(s1, e1), NonEmpty(s2, e2)) =>
      BoundingBox(s1 + s1, e1 + e2)
    case _ => BoundingBox.empty
  
  def - (that: BoundingBox[X]): BoundingBox[X] =
    this + (-that)
    
  def map [Y: Ring: Ordering] (f: Vec[X] <=> Vec[Y]): BoundingBox[Y] = this match
    case NonEmpty(start, end) => BoundingBox(f(start), f(end))
    case _ => BoundingBox.empty
  
  def intervals: Seq[Interval.NonEmpty[X]] = this match
    case NonEmpty(start, end) => start.components.zip(end.components).map(Interval.NonEmpty(_, _))
    case Empty() => Seq.empty
  
  def asUnbounded: BoundingBox[WithInfinity[X]] = this match
    case NonEmpty(start, end) => BoundingBox(start.asUnbounded, end.asUnbounded)
    case Empty() => BoundingBox.empty
  
  def isEmpty: Boolean = this match
    case NonEmpty(_, _) => false
    case Empty() => true
  
  def nonEmpty: Boolean = !isEmpty
    
  def isFinite(using X =:= WithInfinity[?]): Boolean = this match
    case NonEmpty(start, end) => start.isFinite && end.isFinite
    case Empty() => true
  
  def isInfinite(using X =:= WithInfinity[?]): Boolean = !isFinite
  
  def asFinite[Y: Ring: Ordering](using X =:= WithInfinity[Y]): BoundingBox[Y] = this match
    case NonEmpty(start, end) => BoundingBox(start.asFinite, end.asFinite)
    case Empty() => BoundingBox.empty
  
  override def toString = this match
    case NonEmpty(start, end) => s"${size.components.mkString("[", " ⨯ ", "]")} @ $start"
    case Empty() => "∅"
    
object BoundingBox:
  
  type BoundingBoxI = BoundingBox[Int]
  type BoundingBoxL = BoundingBox[Long]
  type BoundingBoxF = BoundingBox[Float]
  type BoundingBoxD = BoundingBox[Double]

  type UBoundingBox[X] = BoundingBox[WithInfinity[X]]
  type UBoundingBoxI = BoundingBox[ExtendedInt]
  type UBoundingBoxL = BoundingBox[ExtendedLong]
  type UBoundingBoxF = BoundingBox[ExtendedFloat]
  type UBoundingBoxD = BoundingBox[ExtendedDouble]
  
  type HasBoundingBoxI = HasBoundingBox[Int]
  type HasBoundingBoxL = HasBoundingBox[Long]
  type HasBoundingBoxF = HasBoundingBox[Float]
  type HasBoundingBoxD = HasBoundingBox[Double]
  
  type HasUBoundingBox[X] = HasBoundingBox[WithInfinity[X]]
  type HasUBoundingBoxI = HasBoundingBox[ExtendedInt]
  type HasUBoundingBoxL = HasBoundingBox[ExtendedLong]
  type HasUBoundingBoxF = HasBoundingBox[ExtendedFloat]
  type HasUBoundingBoxD = HasBoundingBox[ExtendedDouble]
  
  def apply[X: Ring](start: HasVec[X], end: HasVec[X])(using O: Ordering[X]): BoundingBox[X] =
    if start.position.dim > 0 && end.position.dim > 0 && start.position <= end.position
    then BoundingBox.NonEmpty(start.position, end.position)
    else BoundingBox.Empty()
    
  def point[X: Ring: Ordering](v: Vec[X]): BoundingBox[X] = apply(v, v)
  
  def fromOrigin[X: Ordering](size: HasVec[X])(using R: Ring[X]): BoundingBox[X] =
    if size.position.dim > 0 && size.position >= R.multiplicativeIdentity
    then BoundingBox.NonEmpty(Vec.zero[X](size.position.dim), size.position - Vec.one[X](size.position.dim))
    else BoundingBox.Empty()
    
  def of[X: Ring](vs: HasVec[X]*)(using O: Ordering[X]): BoundingBox[X] =
    BoundingBox.NonEmpty (
      vs.map(_.position).reduceOption(_.zip(_)(O.min)).getOrElse(Vec.empty),
      vs.map(_.position).reduceOption(_.zip(_)(O.max)).getOrElse(Vec.empty),
    )
    
  def empty[X: Ring: Ordering]: BoundingBox[X] =
    BoundingBox.Empty()
  
  trait HasBoundingBox[X: Ring: Ordering]:
    def boundingBox: BoundingBox[X]
    def inBounds(v: HasVec[X]): Boolean = boundingBox.inBounds(v)
    def inBounds[Y](using X =:= WithInfinity[Y])(v: HasVec[Y]): Boolean =
      inBounds(v.position.asUnbounded.asInstanceOf[Vec[X]])
    def inBounds(v: X*): Boolean = boundingBox.inBounds(v*)
    def size: Vec[X] = boundingBox.size
    def dim: Int = boundingBox.dim
  
  object BoundingBoxI:
    def empty: BoundingBoxI = BoundingBox.empty[Int]
    export BoundingBox.{empty => _, *}
  
  object BoundingBoxL:
    def empty: BoundingBoxL = BoundingBox.empty[Long]
    export BoundingBox.{empty => _, *}
  
  object BoundingBoxF:
    def empty: BoundingBoxF = BoundingBox.empty[Float]
    export BoundingBox.{empty => _, *}
  
  object BoundingBoxD:
    def empty: BoundingBoxD = BoundingBox.empty[Double]
    export BoundingBox.{empty => _, *}
  
  object UBoundingBoxI:
    def empty: UBoundingBoxI = BoundingBox.empty[ExtendedInt]
    export BoundingBox.{empty => _, *}
  
  object UBoundingBoxL:
    def empty: UBoundingBoxL = BoundingBox.empty[ExtendedLong]
    export BoundingBox.{empty => _, *}
  
  object UBoundingBoxF:
    def empty: UBoundingBoxF = BoundingBox.empty[ExtendedFloat]
    export BoundingBox.{empty => _, *}
  
  object UBoundingBoxD:
    def empty: UBoundingBoxD = BoundingBox.empty[ExtendedDouble]
    export BoundingBox.{empty => _, *}