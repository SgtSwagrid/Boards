package boards.math.vector

import boards.math.algebra.Algebra.{*, given}
import boards.math.Number.gcd
import boards.math.algebra.Unbounded.{Finite, UDouble, UFloat, UInt, ULong, URational, given}
import boards.math.vector.Metric.EnumerableMetric
import boards.math.vector.Region.{HasRegion, RegionI}
import boards.math.vector.Vec.{HasVec, UVec, VecF, VecI}
import boards.math.{*, given}
import boards.math.algebra.Unbounded
import boards.math.ops.AffineOps.*
import boards.math.ops.MinkowskiOps.Difference
import io.circe.*

import scala.annotation.targetName
import scala.compiletime.erasedValue
import scala.math.Ordered.orderingToOrdered
import scala.runtime.RichInt
import scala.util.Random

/**
 * A dense geometric vector.
 * @tparam X The ring over which the components of this vector are defined.
 */
trait Vec [@specialized X: Numeric as R]
extends Region[X], Affine[X, Vec[X]]:
  
  val position: Vec[X] = this
  def contains (v: Vec[X]): Boolean = this == v
  lazy val positions: LazyList[Vec[X]] = LazyList(this)
  def bounds: Bounds[X] = Bounds.point(this)
  
  // BASIC OPERATIONS
  
  /** The dimensionality (number of components) of this vector. */
  def dim: Int
  
  /** The n-th component of this vector in the standard basis. */
  def apply (axis: Int): X
  
  /** All components of this vector in the standard basis. */
  def components: IndexedSeq[X]
  
  def default: X
  def withDefault (x: X): Vec[X]
  
  /** An iterator over all components of this vector in the standard basis. */
  def iterator: Iterator[X] = components.iterator
  
  /** Transform each of the components of this vector independently. */
  def map [Y: Numeric] (f: X => Y): Vec[Y] =
    Vec(components.map(f)).withDefault(f(default))
  
  /** Merge two vectors component-wise. */
  def zip [Y: Numeric, Z: Numeric] (v: Vec[Y]) (f: (X, Y) => Z): Vec[Z] =
    Vec(components.zipAll(v.components, default, v.default).map(f.tupled))
  
  /** Whether this vector is 0-dimensional. */
  override def isEmpty: Boolean = dim == 0
  /** Whether this vector is not 0-dimensional. */
  override def nonEmpty: Boolean = dim > 0
  /** Whether all components of this vector are zero. */
  def isZero: Boolean = forall(_ == R.zero)
  
  // INFIX OPERATORS
  
  /** Add two vectors component-wise. */
  override def translate (offset: Vec[X]): Vec[X] = zip(offset)(_ + _)
  
  override infix def + (that: Vec[X]): Vec[X] = translate(that)
  override infix def + (that: Region[X]): Region[X] = that.translate(this)
  override infix def - (that: Vec[X]): Vec[X] = this + (-that)
  override infix def - (that: Region[X]): Region[X] = this + (-that)
  override def negate: Vec[X] = map(-_)
  override def unary_- : Vec[X] = map(-_)
  
  /** Multiply two vectors component-wise. */
  override def scale (v: Vec[X]): Vec[X] = zip(v)(_ * _)
  
  def / (x: X): Vec[X] = map(_ / x)
  /** Divide two vectors component-wise. */
  def / (v: Vec[X]): Vec[X] = zip(v)(_ / _)
  
  // ALIASES
  
  /** The first component of this vector; equivalent to `apply(0)`. */
  def x: X = apply(0)
  /** The second component of this vector; equivalent to `apply(1)`. */
  def y: X = apply(1)
  /** The third component of this vector; equivalent to `apply(2)`. */
  def z: X = apply(2)
  
  /** The first component of this vector; equivalent to `apply(0)`. */
  def width: X = apply(0)
  /** The second component of this vector; equivalent to `apply(1)`. */
  def height: X = apply(1)
  /** The third component of this vector; equivalent to `apply(2)`. */
  def depth: X = apply(2)
  
  def aspectRatio: X = abs.x / abs.y
  
  // INEQUALITIES
  
  /** Whether each component of the first vector is always strictly less than that of the second. */
  def < (v: Vec[X]): Boolean = zip(v)(_ < _).foldLeft(true)(_ & _)
  /** Whether all components of the vector are strictly less than some constant. */
  def < (x: X): Boolean = forall(_ < x)
  /** Whether each component of the first vector is always less than or equal to that of the second. */
  def <= (v: Vec[X]): Boolean = zip(v)(_ <= _).foldLeft(true)(_ & _)
  /** Whether all components of the vector are less than or equal to some constant. */
  def <= (x: X): Boolean = forall(_ <= x)
  /** Whether each component of the first vector is always strictly greater than that of the second. */
  def > (v: Vec[X]): Boolean = zip(v)(_ > _).foldLeft(true)(_ & _)
  /** Whether all components of the vector are strictly greater than some constant. */
  def > (x: X): Boolean = forall(_ > x)
  /** Whether each component of the first vector is always greater than or equal to that of the second. */
  def >= (v: Vec[X]): Boolean = zip(v)(_ >= _).foldLeft(true)(_ & _)
  /** Whether all components of the vector are greater than or equal to some constant. */
  def >= (x: X): Boolean = forall(_ >= x)
  
  /** A transformed version of this vector with the absolute value applied to all components. */
  def abs: Vec[X] = map(x => if x < R.zero then -x else x)
  
  // GEOMETRIC OPERATIONS
  
  /** The dot product (sum of pair-wise products) between two vectors. */
  infix def dot (v: Vec[X]): X = zip(v)(_ * _).sum
  
  /** The cross product between two 3D vectors. */
  infix def cross (v: Vec[X]): Vec[X] = Vec (
    y * v.z - z * v.y,
    z * v.x - x * v.z,
    x * v.y - y * v.x,
  )
  
  /** A transformed version of this vector flipped over the given axis. */
  override def flip (axis: Int): Vec[X] = update(axis, -apply(axis))
  
  /** A transformed version of this vector rotated 90 degrees from the `from`-axis to the `to`-axis. */
  override def rotate (from: Int, to: Int): Vec[X] = update(to, apply(from)).update(from, -apply(to))
  
  /** Project this vector onto another. */
  def proj (axis: Vec[X]) (using Metric[X], Dividable[X]): Vec[X] =
    (this dot axis.normalise) * axis.normalise
    
  /** Project this vector onto the given axis, i.e. replace all other components with 0. */
  def proj (axis: Int): Vec[X] = Vec.zero(dim).update(axis, apply(axis))
  /** Project this vector onto the X-axis, i.e. replace all other components with 0. */
  def projX: Vec[X] = proj(0)
  /** Project this vector onto the Y-axis, i.e. replace all other components with 0. */
  def projY: Vec[X] = proj(1)
  /** Project this vector onto the Z-axis, i.e. replace all other components with 0. */
  def projZ: Vec[X] = proj(2)
  
  // MODIFICATIONS
  
  /** A copy of this vector with the `axis`-th component replaced with `x`. */
  def update (axis: Int, x: X): Vec[X] =
    takeLeft(axis) ++ Vec.zero(axis - dim) ++ Vec(x) ++ dropLeft(axis + 1)
  
  /** Sample the vector at the given `axes`. */
  def swizzle (axes: Int*): Vec[X] = Vec(axes.map(apply))
  
  /** Linearly transform this vector so that the basis vectors are mapped to the given `dirs`. */
  def transform (dirs: Vec[X]*): Vec[X] =
    iterator.zip(dirs).map(_ * _).foldLeft(Vec.empty)(_ + _)
  
  /** Restrict all components of this vector to be non-negative. */
  def clamp: Vec[X] = clamp(R.zero)
  /** Restrict all components of this vector to be at least `min`. */
  def clamp (min: X = R.zero): Vec[X] =
    map(x => if x < min then min else x)
  /** Restrict all components of this vector to be between `min` and `max` inclusive.  */
  def clamp (min: X, max: X): Vec[X] =
    map(x => if x < min then min else if x > max then max else x)
  
  // LIST OPERATIONS
  
  /** Retain only the components from the `from`-axis (inclusive) to the `to`-axis (exclusive). */
  def slice (from: Int, until: Int): Vec[X] = Vec(components.slice(from, until))
  /** Append the components of 2 vectors end-to-end. */
  def ++ (v: Vec[X]): Vec[X] = Vec(components ++ v.components)
  /** Append an element to the start of this vector. */
  def +: (x: X): Vec[X] = Vec(x +: components)
  /** Append an element to the end of this vector. */
  def :+ (x: X): Vec[X] = Vec(components :+ x)
  
  /** Keep only the first `n` components. */
  def takeLeft (n: Int): Vec[X] = slice(0, n)
  /** Discard the first `n` components. */
  def dropLeft (n: Int): Vec[X] = slice(n, dim)
  /** Keep only the last `n` components. */
  def takeRight (n: Int): Vec[X] = slice(dim - n, dim)
  /** Discard the last `n` components. */
  def dropRight (n: Int): Vec[X] = slice(0, dim - n)
  
  /** A left-to-right list reduction operation. */
  def foldLeft [Y] (y: Y) (f: (Y, X) => Y): Y = iterator.foldLeft(y)(f)
  /** A right-to-left list reduction operation. */
  def foldRight [Y] (y: Y) (f: (X, Y) => Y): Y = iterator.foldRight(y)(f)
  
  /** Find the sum of all components. */
  def sum: X = foldLeft(R.zero)(_ + _)
  /** Find the product of all components. */
  def product: X = foldLeft(R.one)(_ * _)
  
  /** Whether the given `f` is true for all components. True by default if empty. */
  def forall (f: X => Boolean): Boolean = map(f).foldLeft(true)(_ & _)
  /** Whether the given `f` is true for some component. False by default if empty. */
  def exists (f: X => Boolean): Boolean = map(f).foldLeft(false)(_ | _)
  
  /** Whether this vector is inside the given region. */
  infix def in (region: Region[X]): Boolean =
    region.contains(this)
  
  /** Whether all components of this vector have finite values. */
  def isFinite [Y] (using X =:= Unbounded[Y]): Boolean = forall(_.isFinite)
  
  /** Whether some component of this vector has an infinite value. */
  def isInfinite [Y] (using X =:= Unbounded[Y]): Boolean = exists(_.isInfinite)
  
  def toFinite [Y: Numeric] (using X =:= Unbounded[Y]): Vec[Y] =
    map(_.toFinite)
    
  def toUnbounded: UVec[X] =
    map(Finite.apply)
    
  def toFiniteOption [Y: Numeric] (using X =:= Unbounded[Y]): Option[Vec[Y]] =
    Option.when(isFinite)(toFinite)
  
  override def toString: String = iterator.mkString("[", ", ", "]")
  
  override def equals (that: Any): Boolean = that match
    // Two vectors are equal if all of their components are.
    case v: Vec[?] => components == v.components
    case _ => false
    
  override def hashCode: Int =
    // Temporary: a simple hash function for vectors of small component size.
    components.foldLeft(0)((hash, x) => hash << 4 + x.hashCode)
    
  def direction (using X =:= Int): VecI =
    val divisor = gcd(asVecI.abs.components*)
    asVecI.map(_ / divisor)
  
  /** The relative direction between two `Vec`s, divided by the GCD of its components. */
  def directionTo (u: VecI) (using X =:= Int): VecI =
    (u - asVecI).direction
  
  /** The number of steps it takes to get from `v` to `u` if using `v.directionTo(u)` on each step. */
  def stepsTo (u: VecI) (using X =:= Int): Int =
    (u - asVecI).asMultipleOf(asVecI.directionTo(u)).get
  
  /** @return If `v` is an integer multiple of `u`, `Some(a)` s.t. `v=au`, otherwise `None`. */
  def asMultipleOf (u: VecI) (using X =:= Int): Option[Int] =
    if isZero then Some(0)
    else if components.zip(u.components).exists(_ == 0 ^ _ == 0) then None
    else
      val pairs = asVecI.components.filter(_ != 0).zip(u.components.filter(_ != 0))
      if pairs.exists(_ % _ != 0) then None else
        pairs.headOption match
          case None => Some(0)
          case Some(v0, u0) =>
            val ratio = v0 / u0
            if pairs.forall(_ / _ == ratio) then Some(ratio) else None
  
  /**
   * Create a new ray consisting of all points on the line between two `Vec`s, including the end.
   * Each step is determined using `VecI.directionTo`.
   */
  def rayTo (to: VecI) (using X =:= Int): Ray =
    Ray.between(asVecI, to)
  
  /**
   * Create a new ray consisting of all points on the line between two `Vec`s, excluding the end.
   * Each step is determined using `VecI.directionTo`.
   */
  def rayUntil (until: VecI) (using X =:= Int): Ray =
    rayTo(until).retract(1)
  
  /**
   * Create a ray consisting of all points along some direction(s) from this `Vec`.
   *
   * @param dir The direction(s) in which the ray extends.
   * @param inclusive Whether this vertex is included (default=false).
   */
  def ray (dir: RegionI, inclusive: Boolean = false) (using X =:= Int): Ray =
    Ray.from(asVecI, dir, inclusive)
  
  /** The arithmetic mean of two `VecI`s. May produce rounding artifacts. */
  def midpoint (u: VecI) (using X =:= Int): VecI = VecI.midpoint(asVecI, u)
  
  private[math] def asVecI (using X =:= Int): VecI =
    this.asInstanceOf[VecI]
  
object Vec:
  
  type VecI = Vec[Int]
  type VecL = Vec[Long]
  type VecF = Vec[Float]
  type VecD = Vec[Double]
  type VecR = Vec[Rational]
  
  type UVec [X] = Vec[Unbounded[X]]
  type UVecI = Vec[UInt]
  type UVecL = Vec[ULong]
  type UVecF = Vec[UFloat]
  type UVecD = Vec[UDouble]
  type UVecR = Vec[URational]
  
  /** The canonical `Vec` implementation, backed by an `IndexedSeq`. */
  private class SeqVec [@specialized X: Numeric as R] (
    val components: IndexedSeq[X],
    val default: X,
  ) extends Vec[X]:
    def apply (n: Int): X = if components.isDefinedAt(n) then components(n) else R.additiveIdentity
    override val dim: Int = components.size
    def withDefault (default: X): SeqVec[X] = SeqVec(components, default)
    
  /** Construct a new `Vec` with the given components in the standard basis. */
  def apply [X: Numeric as N] (x: X*): Vec[X] = SeqVec(x.toIndexedSeq, N.zero)
  /** Construct a new `Vec` with the given components in the standard basis. */
  def apply [X: Numeric as N] (x: Iterable[X]): Vec[X] = SeqVec(x.toIndexedSeq, N.zero)
  
  /** Construct a `d`-dimensional `Vec` of all 0's, where 0 is the additive identity. */
  def zero [X: Numeric as R] (d: Int = 0): Vec[X] =
    Vec(Seq.fill(d)(R.additiveIdentity))
  /** The unique 0-dimensional `Vec` over the ring `X`. */
  def empty [X: Numeric]: Vec[X] = zero(0)
  
  /** Construct a `d`-dimensional `Vec` of all 1's, where 1 is the multiplicative identity. */
  def one [X: Numeric as R] (d: Int = 0): Vec[X] =
    Vec(Seq.fill(d)(R.one))
  
  /**
   * Construct a `d`-dimensional `Vec` of all 0's except in the `n`-th component, where there is a 1.
   * If `d` < `n`, then the value of `d` given is ignored and taken to be `n` instead.
   */
  def axis [X: Numeric as R] (n: Int, d: Int = 0): Vec[X] =
    Vec:
      Seq.fill(n)(R.zero) :+
      R.one :++
      Seq.fill(d - n - 1)(R.zero)
  
  /** Construct a `d`-dimensional `Vec` where all components have the same, given value. */
  def fill [X: Numeric] (d: Int) (x: X): Vec[X] = Vec(Seq.fill(d)(x))
  
  def fillForever [X: Numeric] (x: X): Vec[X] = Vec.empty.withDefault(x)
    
  // If a metric is defined for this vector space:
  extension [X: {Numeric, Metric as M}] (v: Vec[X])
    
    /** The length of this vector according to the current `Metric`. */
    def norm: X = M.norm(v)
    
    /** Normalise this vector to have length 1 according to the current `Metric`. */
    def normalise (using Dividable[X]): Vec[X] = v / v.norm
    
    /** The distance between two vectors. */
    def dist (u: Vec[X]): X = M.dist(v, u)
    
  // If this vector space has some notion of adjacency:
  extension [X: {Numeric, EnumerableMetric as M}] (v: Vec[X])
    
    /** Whether these vectors are adjacent. */
    def adjacent (u: Vec[X]): Boolean = M.adjacent(v, u)
    
    /** Enumerate all vectors which are adjacent to this one. */
    //def neighbours: Region[X] = M.neighbours(v)
  
  object VecI:
    
    /** Construct a new `VecI` with the given components in the standard basis. */
    def apply (A: Int*): VecI = Vec[Int](A)
    /** Construct a new `VecI` with the given components in the standard basis. */
    def apply (A: Iterable[Int]): VecI = Vec[Int](A)
    
    /** Construct a `d`-dimensional `VecI` of all 0's. */
    def zero (d: Int = 0): VecI = Vec.zero[Int](d)
    
    /** The unique 0-dimensional `VecI`. */
    def empty: VecI = Vec.empty[Int]
    
    /** Construct a `d`-dimensional `VecI` of all 1's. */
    def one (d: Int = 0): VecI = Vec.one[Int](d)
    
    /**
     * Construct a `d`-dimensional `VecI` of all 0's except in the `n`-th component, where there is a 1.
     * If `d` < `n`, then the value of `d` given is ignored and taken to be `n` instead.
     */
    def axis (n: Int, d: Int = 0): VecI = Vec.axis[Int](n, d)
    
    /** Construct a `d`-dimensional `VecI` where all components have the same, given value. */
    def fill (d: Int) (x: Int): VecI = Vec.fill(d)(x)
    
    /** The arithmetic mean of the given `VecI`s. May produce rounding artifacts. */
    def midpoint (v: VecI*): VecI = v.reduce(_ + _) / v.size
      
  // `Vec[X]` forms a `Group` for any `Ring` `X`.
  given [X: Numeric]: OrderedGroup[Vec[X]] with
    
    def sum (x: Vec[X], y: Vec[X]): Vec[X] = x + y
    def additiveIdentity: Vec[X] = Vec.empty[X]
    def additiveInverse (x: Vec[X]): Vec[X] = -x
    
    def compare (x: Vec[X], y: Vec[X]): Int = ((x.components, y.components): @unchecked) match
      case (x0 +: _, y0 +: _) if x0.compare(y0) != 0 => x0.compare(y0)
      case (x0 +: xs, y0 +: ys) if x0.compare(y0) == 0 => xs.compare(ys)
      case _ if x.isEmpty && y.isEmpty => 0
      case _ if x.isEmpty => -1
      case _ if y.isEmpty => 1
  
  // By default, `VecF` should use the Euclidean metric.
  given Metric[Float] = Metric.Euclidean
  
  trait HasVec [X: Numeric] extends HasRegion[X]:
    val position: Vec[X]
    def region: Region[X] = Region.point(position)
    
  type HasVecI = HasVec[Int]
  
  type HasUVec[X] = HasVec[Unbounded[X]]
  type HasUVecI = HasUVec[Int]
  
  given [X: Encoder]: Encoder[Vec[X]] =
    Encoder.encodeSeq[X].contramap(v => v.default +: v.components)
  
  given [X: {Decoder, Numeric}]: Decoder[Vec[X]] =
    Decoder.decodeSeq[X].map(s => Vec(s.tail).withDefault(s.head))