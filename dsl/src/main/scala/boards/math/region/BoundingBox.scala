package boards.math.region

import boards.math.Algebra.*
import boards.math.Unbounded.*
import boards.math.region.Vec.HasVec
import boards.math.Bijection.<=>
import boards.math.{Interval, Rational, Surd, Unbounded}
import boards.math.Conversions.*

/** An axis-aligned bounding box in arbitrary-dimensional Euclidean space.
  * Can be thought of as a multidimensional variant of [[Interval]].
  * @tparam X The discrete field over which the bounding box is defined.
  */
enum BoundingBox [@specialized X: OrderedRing as R]:
  
  /** A strictly non-empty bounding box defined by an interval in each dimension.
    * @param start The smallest corner of the bounding box.
    * @param end The largest corner of the bounding box.
    */
  case NonEmpty [Y: OrderedRing] (
    override val start: Vec[Y],
    end: Vec[Y],
  ) extends BoundingBox[Y]
  
  /** An empty bounding box containing no vectors. */
  case Empty [Y: OrderedRing] () extends BoundingBox[Y]
  
  /** The dimensions of this bounding box. */
  val size: Vec[X] = this match
    case NonEmpty(start, end) => end - start + Vec.one(Math.max(start.dim, end.dim))
    case Empty() => Vec.empty
    
  /** The smallest corner of the bounding box, or the origin if it is empty. */
  val start: Vec[X] = this match
    case NonEmpty(start, _) => start
    case Empty() => Vec.empty
  
  export size.{dim, product => area}
  
  /** Whether the given vector lies inside the bounding box. */
  def inBounds (v: HasVec[X]): Boolean = this match
    case NonEmpty(start, end) => start <= v.position && v.position <= end
    case Empty() => false
  
  /** Whether the given vector lies inside the bounding box. */
  def inBounds (v: X*): Boolean = inBounds(Vec(v))
  
  /** Whether the given vector lies inside the bounding box. */
  def inBounds [Y: OrderedRing] (v: HasVec[Y]) (using X =:= Unbounded[Y]): Boolean =
    inBounds(v.position.toUnbounded.asInstanceOf[Vec[X]])
  
  /** Whether the given vector lies inside the bounding box. */
  def inBounds [Y: OrderedRing] (v: Y*) (using X =:= Unbounded[Y]): Boolean =
    inBounds(Vec(v*))
  
  /** Produces the minimal bounding box to contain both operands. */
  def | (that: BoundingBox[X]): BoundingBox[X] = (this, that) match
    case (NonEmpty(s1, e1), NonEmpty(s2, e2)) =>
      BoundingBox(s1.zip(s2)(R.min), e1.zip(e2)(R.max))
    case (left: BoundingBox.NonEmpty[X], BoundingBox.Empty()) => left
    case (Empty(), right: NonEmpty[X]) => right
    case _ => BoundingBox.empty
  
  /** Produces the maximal bounding box contained in both operands. */
  def & (that: BoundingBox[X]): BoundingBox[X] = (this, that) match
    case (NonEmpty(s1, e1), NonEmpty(s2, e2)) =>
      BoundingBox(s1.zip(s2)(R.max), e1.zip(e2)(R.min))
    case _ => BoundingBox.empty
  
  /** Flip the bounding box over all axes. */
  def unary_- : BoundingBox[X] = this match
    case NonEmpty(start, end) => BoundingBox(-end, -start)
    case _ => BoundingBox.empty
  
  /** Take the Minkowski sum of both operands. */
  def + (that: BoundingBox[X]): BoundingBox[X] = (this, that) match
    case (NonEmpty(s1, e1), NonEmpty(s2, e2)) =>
      BoundingBox(s1 + s1, e1 + e2)
    case _ => BoundingBox.empty
  
  /** Take the Minkowski difference of both operands. */
  def - (that: BoundingBox[X]): BoundingBox[X] =
    this + (-that)
  
  def map [Y: OrderedRing] (f: Vec[X] => Vec[Y]): BoundingBox[Y] = this match
    case NonEmpty(start, end) => BoundingBox(f(start), f(end))
    case _ => BoundingBox.empty
    
  def mapComponents [Y: OrderedRing] (f: X => Y): BoundingBox[Y] =
    this.map(_.map(f))
    
  /** Transform this bounding box with an affine bijection. */
  def bimap [Y: OrderedRing] (f: Vec[X] <=> Vec[Y]): BoundingBox[Y] = this match
    case NonEmpty(start, end) => BoundingBox(f(start), f(end))
    case _ => BoundingBox.empty
    
  def translate (v: Vec[X]): BoundingBox[X] = this match
    case NonEmpty(start, end) => BoundingBox(start + v, end + v)
    case _ => BoundingBox.empty
  
  def extend (v: Vec[X]): BoundingBox[X] = this match
    case NonEmpty(start, end) => BoundingBox(start - v, end + v)
    case _ => BoundingBox.empty
    
  def extend (x: X): BoundingBox[X] = extend(Vec.fill(dim)(x))
  
  def extend (dim: Int) (x: X): BoundingBox[X] = extend(Vec.axis(dim, this.dim) * x)
  
  def extendStart (v: Vec[X]): BoundingBox[X] = this match
    case NonEmpty(start, end) => BoundingBox(start - v, end)
    case _ => BoundingBox.empty
    
  def extendStart (x: X): BoundingBox[X] = extendStart(Vec.fill(dim)(x))
  
  def extendStart (dim: Int) (x: X): BoundingBox[X] = extendStart(Vec.axis(dim, this.dim) * x)
  
  def extendEnd (v: Vec[X]): BoundingBox[X] = this match
    case NonEmpty(start, end) => BoundingBox(start, end + v)
    case _ => BoundingBox.empty
    
  def extendEnd (x: X): BoundingBox[X] = extendEnd(Vec.fill(dim)(x))
  
  def extendEnd (dim: Int) (x: X): BoundingBox[X] = extendEnd(Vec.axis(dim, this.dim) * x)
  
  /** Represent the bounds as an interval in each axis. */
  lazy val intervals: Seq[Interval.NonEmpty[X]] = this match
    case NonEmpty(start, end) => start.components.zip(end.components).map(Interval.NonEmpty(_, _))
    case Empty() => Seq.empty
  
  /** Whether this bounding box contains no vectors. */
  def isEmpty: Boolean = this match
    case NonEmpty(_, _) => false
    case Empty() => true
  
  /** Whether this bounding box contains at least one vector. */
  def nonEmpty: Boolean = !isEmpty
  
  /** Whether this bounding box is of finite size in all dimensions. */
  def isFinite (using X =:= Unbounded[?]): Boolean = this match
    case NonEmpty(start, end) => start.isFinite && end.isFinite
    case Empty() => true
  
  /** Whether this bounding box is of infinite size in some dimension. */
  def isInfinite (using X =:= Unbounded[?]): Boolean = !isFinite
  
  override def toString = this match
    case NonEmpty(start, end) => s"${size.components.mkString("[", " ⨯ ", "]")} @ $start"
    case Empty() => "∅"
    
object BoundingBox:
  
  type BoundingBoxI = BoundingBox[Int]
  type BoundingBoxL = BoundingBox[Long]
  type BoundingBoxF = BoundingBox[Float]
  type BoundingBoxD = BoundingBox[Double]
  type BoundingBoxR = BoundingBox[Rational]
  type BoundingBoxS = BoundingBox[Surd]

  type UBoundingBox[X] = BoundingBox[Unbounded[X]]
  type UBoundingBoxI = BoundingBox[UInt]
  type UBoundingBoxL = BoundingBox[ULong]
  type UBoundingBoxF = BoundingBox[UFloat]
  type UBoundingBoxD = BoundingBox[UDouble]
  type UBoundingBoxR = BoundingBox[URational]
  type UBoundingBoxS = BoundingBox[USurd]
  
  type HasBoundingBoxI = HasBoundingBox[Int]
  type HasBoundingBoxL = HasBoundingBox[Long]
  type HasBoundingBoxF = HasBoundingBox[Float]
  type HasBoundingBoxD = HasBoundingBox[Double]
  
  type HasUBoundingBox[X] = HasBoundingBox[Unbounded[X]]
  type HasUBoundingBoxI = HasBoundingBox[UInt]
  type HasUBoundingBoxL = HasBoundingBox[ULong]
  type HasUBoundingBoxF = HasBoundingBox[UFloat]
  type HasUBoundingBoxD = HasBoundingBox[UDouble]
  
  /** Create a bounding box between the given start and end, which will be empty if {{{!(end >= start)}}}. */
  def apply [X: OrderedRing] (start: HasVec[X], end: HasVec[X]): BoundingBox[X] =
    if start.position.dim > 0 && end.position.dim > 0 && start.position <= end.position
    then BoundingBox.NonEmpty(start.position, end.position)
    else BoundingBox.Empty()
    
  /** Create a unit bounding box containing only a single vector. */
  def point [X: OrderedRing] (v: Vec[X]): BoundingBox[X] = apply(v, v)
  
  /** Create a bounding box from the origin in the positive orthant of the given size. */
  def fromOrigin [X: OrderedRing as R] (size: HasVec[X]): BoundingBox[X] =
    if size.position.dim > 0 && size.position >= R.multiplicativeIdentity
    then BoundingBox.NonEmpty(Vec.zero[X](size.position.dim), size.position - Vec.one[X](size.position.dim))
    else BoundingBox.Empty()
    
  /** Create the minimal bounding box to contain all the given points. */
  def bounding [X: OrderedRing as O] (vs: HasVec[X]*): BoundingBox[X] =
    BoundingBox.NonEmpty (
      vs.map(_.position).reduceOption(_.zip(_)(O.min)).getOrElse(Vec.empty),
      vs.map(_.position).reduceOption(_.zip(_)(O.max)).getOrElse(Vec.empty),
    )
    
  /** Create an empty bounding box. */
  def empty [X: OrderedRing]: BoundingBox[X] =
    BoundingBox.Empty()
  
  trait HasBoundingBox [X: OrderedRing]:
    def boundingBox: BoundingBox[X]
    //def finiteBoundingBox [Y: OrderedRing] (using X =:= Unbounded[Y]): BoundingBox[Y] =
    //  boundingBox.toBounded.asInstanceOf[BoundingBox[Y]]
    def inBounds (v: HasVec[X]): Boolean = boundingBox.inBounds(v)
    def inBounds [Y: OrderedRing] (using X =:= Unbounded[Y]) (v: HasVec[Y]): Boolean =
      inBounds(v.position.toUnbounded.asInstanceOf[Vec[X]])
    def inBounds (v: X*): Boolean = boundingBox.inBounds(v*)
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
    def empty: UBoundingBoxI = BoundingBox.empty[UInt]
    export BoundingBox.{empty => _, *}
  
  object UBoundingBoxL:
    def empty: UBoundingBoxL = BoundingBox.empty[ULong]
    export BoundingBox.{empty => _, *}
  
  object UBoundingBoxF:
    def empty: UBoundingBoxF = BoundingBox.empty[UFloat]
    export BoundingBox.{empty => _, *}
  
  object UBoundingBoxD:
    def empty: UBoundingBoxD = BoundingBox.empty[UDouble]
    export BoundingBox.{empty => _, *}