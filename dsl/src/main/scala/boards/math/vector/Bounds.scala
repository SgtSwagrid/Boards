package boards.math.vector

import boards.math.algebra.Algebra.*
import boards.math.vector.Vec
import boards.math.{Interval, Rational}
import boards.math.Conversions.*
import boards.math.ops.AffineOps.*
import boards.math.vector.Vec.UVec
import boards.math.algebra.Unbounded.Finite
import boards.math.algebra.Unbounded
import boards.math.ops.SetOps.{Intersection, Union}
import boards.math.ops.TransformOps.UVecFunctor
import boards.math.ops.MinkowskiOps.Difference
import io.circe.*

/** An axis-aligned bounding box in arbitrary-dimensional Euclidean space.
  * Can be thought of as a multidimensional variant of [[Interval]].
  * @tparam X The discrete field over which the bounding box is defined.
  */
case class Bounds [@specialized X: Numeric as R] (
  intervals: IndexedSeq[Interval[X]]
) extends
  Affine[X, Bounds[X]],
  Difference[Bounds[X]],
  Union[Bounds[X]],
  Intersection[Bounds[X]],
  UVecFunctor[X, Bounds]:
  
  val dim: Int = intervals.size
    
  /** The smallest corner of the bounding box. */
  val ustart: UVec[X] = Vec(intervals.map(_.ustart))
  def start: Vec[X] = ustart.toFinite[X]
    
  /** The largest corner of the bounding box. */
  val uend: UVec[X] = Vec(intervals.map(_.uend))
  def end: Vec[X] = uend.toFinite[X]
  
  /** The dimensions of this bounding box. */
  val usize: UVec[X] = Vec(intervals.map(_.ulength))
  def size: Vec[X] = usize.toFinite[X]
  
  val uarea: Unbounded[X] = usize.product
  def area: X = uarea.toFinite
  
  /** Whether the given vector lies inside the bounding box. */
  def contains (v: Vec[X]): Boolean =
    dim == v.dim &&
      (intervals zip v.components).forall(_.contains(_))
    
  def contains (that: Bounds[X]): Boolean =
    dim == that.dim &&
      (intervals zip that.intervals).forall(_.contains(_))
  
  def map [Y: Numeric] (f: UVec[X] => UVec[Y]): Bounds[Y] =
    Bounds.ubounding(f(ustart), f(uend))
    
  def mapIntervals [Y: Numeric] (f: Interval[X] => Interval[Y]): Bounds[Y] =
    Bounds(intervals.map(f))
  
  /** Produces the minimal bounding box to contain both operands. */
  def union (that: Bounds[X]): Bounds[X] = Bounds:
    (intervals zip that.intervals).map(_ | _)
  
  /** Produces the maximal bounding box contained in both operands. */
  def intersect (that: Bounds[X]): Bounds[X] = Bounds:
    (intervals zip that.intervals).map(_ & _)
  
  /** Take the Minkowski sum of both operands. */
  def minkowskiSum (that: Bounds[X]): Bounds[X] = Bounds:
    (intervals zip that.intervals).map(_ + _)
  
  def negate: Bounds[X] = Bounds:
    intervals.map(_.negate)
  
  def translate (v: Vec[X]): Bounds[X] = map(_ + v.toUnbounded)
  
  def flip (axis: Int): Bounds[X] = map(-_)
  
  def rotate (from: Int, to: Int): Bounds[X] = map(_.rotate(from, to))
  
  def scale (factors: Vec[X]): Bounds[X] = map(_ * factors.toUnbounded)
  
  def extend (v: Vec[X]): Bounds[X] = Bounds:
    (intervals zip v.components).map(_.extend(_))
    
  def extend (x: X): Bounds[X] = Bounds:
    intervals.map(_.extend(x))
  
  def extend (dim: Int) (x: X): Bounds[X] =
    extend(Vec.axis(dim, this.dim) * x)
  
  def extendStart (v: Vec[X]): Bounds[X] = Bounds:
    (intervals zip v.components).map(_.extendStart(_))
    
  def extendStart (x: X): Bounds[X] = Bounds:
    intervals.map(_.extendStart(x))
  
  def extendStart (dim: Int) (x: X): Bounds[X] =
    extendStart(Vec.axis(dim, this.dim) * x)
  
  def extendEnd (v: Vec[X]): Bounds[X] = Bounds:
    (intervals zip v.components).map(_.extendEnd(_))
    
  def extendEnd (x: X): Bounds[X] = Bounds:
    intervals.map(_.extendEnd(x))
  
  def extendEnd (dim: Int) (x: X): Bounds[X] =
    extendEnd(Vec.axis(dim, this.dim) * x)
  
  /** Whether this bounding box contains no vectors. */
  def isEmpty: Boolean = intervals.isEmpty || intervals.exists(_.isEmpty)
  
  /** Whether this bounding box contains at least one vector. */
  def nonEmpty: Boolean = !isEmpty
  
  /** Whether this bounding box is of finite size in all dimensions. */
  def isFinite: Boolean = isEmpty || intervals.forall(_.isFinite)
  
  /** Whether this bounding box is of infinite size in some dimension. */
  def isInfinite: Boolean = !isFinite
  
  def uleft: Unbounded[X] = ustart(0)
  def left: X = ustart(0).toFinite
  def uright: Unbounded[X] = uend(0)
  def right: X = uend(0).toFinite
  def ubottom: Unbounded[X] = ustart(1)
  def bottom: X = ustart(1).toFinite
  def utop: Unbounded[X] = uend(1)
  def top: X = uend(1).toFinite
  
  def bottomLeft: Vec[X] = Vec(left, bottom)
  def topLeft: Vec[X] = Vec(left, top)
  def bottomRight: Vec[X] = Vec(right, bottom)
  def topRight: Vec[X] = Vec(right, top)
  
  def ucentre: UVec[X] = (ustart + uend) / Finite(R.two)
  def centre: Vec[X] = (ustart + uend).toFinite[X] / R.two
  def centred: Bounds[X] = this - centre
  def collapseToCentre: Bounds[X] = Bounds.point(centre)
  def scaleCentred (factor: X): Bounds[X] = (centred * factor) + centre
  def scaleCentred (factors: Vec[X]): Bounds[X] = (centred * factors) + centre
  def directionTo (that: Bounds[X]): Vec[X] = that.centre - this.centre
  
  def width: X = size(0)
  def height: X = size(1)
  def depth: X = size(2)
  
  override def toString = s"$ustart : $uend"
    
object Bounds:
  
  type BoundsI = Bounds[Int]
  type BoundsL = Bounds[Long]
  type BoundsF = Bounds[Float]
  type BoundsD = Bounds[Double]
  type BoundsR = Bounds[Rational]
  
  trait HasBounds [X: Numeric]:
    def bounds: Bounds[X]
    def inBounds (v: Vec[X]): Boolean = bounds.contains(v)
    def usize: UVec[X] = bounds.usize
    def size: Vec[X] = usize.toFinite[X]
    def dim: Int = bounds.dim
    
  given [X]: Conversion[HasBounds[X], Bounds[X]] = _.bounds
  
  /** Create a bounding box between the given start and end, which will be empty if {{{!(end >= start)}}}. */
  def between [X: Numeric] (start: Vec[X], end: Vec[X]): Bounds[X] =
    Bounds((start.components zip end.components).map(Interval.between(_, _)))
    
  def ubetween [X: Numeric] (start: UVec[X], end: UVec[X]): Bounds[X] =
    Bounds((start.components zip end.components).map(Interval.ubetween(_, _)))
  
  /** Create an empty bounding box. */
  def empty [X: Numeric]: Bounds[X] =
    Bounds(IndexedSeq.empty)
    
  def all [X: Numeric] (dim: Int): Bounds[X] =
    Bounds(IndexedSeq.fill(dim)(Interval.all))
    
  /** Create a unit bounding box containing only a single vector. */
  def point [X: Numeric] (v: Vec[X]): Bounds[X] =
    Bounds(v.components.map(Interval.point))
    
  /** Create the minimal bounding box to contain all the given points. */
  def bounding [X: Numeric as R] (vs: Vec[X]*): Bounds[X] =
    Bounds.between (
      vs.map(_.position).reduceOption(_.zip(_)(R.min)).getOrElse(Vec.empty),
      vs.map(_.position).reduceOption(_.zip(_)(R.max)).getOrElse(Vec.empty),
    )
    
  def ubounding [X: Numeric] (vs: UVec[X]*): Bounds[X] =
    val O = summon[Ordering[Unbounded[X]]]
    Bounds.ubetween (
      vs.map(_.position).reduceOption(_.zip(_)(O.min)).getOrElse(Vec.empty),
      vs.map(_.position).reduceOption(_.zip(_)(O.max)).getOrElse(Vec.empty),
    )
    
  /** Create a bounding box from the origin in the positive orthant of the given size. */
  def fromOrigin [X: Numeric as R] (size: Vec[X]): Bounds[X] =
    Bounds.between(Vec.zero(size.dim), size - Vec.one(size.dim))
    
  given [X: {Numeric, Encoder}]: Encoder[Bounds[X]] =
    summon[Encoder[Seq[Interval[X]]]]
      .contramap(_.intervals)
    
  given [X: {Numeric, Decoder}]: Decoder[Bounds[X]] =
    summon[Decoder[IndexedSeq[Interval[X]]]]
      .map(new Bounds(_))