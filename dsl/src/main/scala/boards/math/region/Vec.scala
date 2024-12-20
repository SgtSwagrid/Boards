package boards.math.region

import Metric.*
import boards.math.Number.gcd
import Region.{HasRegion, HasRegionI, RegionI}
import boards.math.WithInfinity.{*, given}
import boards.math.Algebra.{*, given}
import boards.math.WithInfinity
import boards.math.WithInfinity.ExtendedInt
import boards.math.region.BoundingBox.BoundingBoxI
import boards.math.region.Vec.{Dividable, HasVec, HasVecI, UVec, UVecI, VecF, VecI, given}

import scala.annotation.targetName
import scala.compiletime.erasedValue
import scala.math.Ordered.orderingToOrdered
import scala.runtime.RichInt
import scala.util.Random

/**
 * A dense geometric vector.
 * @tparam X The ring over which the components of this vector are defined.
 */
trait Vec [@specialized X: Ring: Ordering] extends HasVec[X]:
  
  val position: Vec[X] = this
  
  // temporary: operators for region
  def | (region: HasRegion[X]): Region[X] = region.region | this
  def & (region: HasRegion[X]): Region[X] = region.region & this
  def \ (region: HasRegion[X]): Region[X] = this.region \ region.region
  def ^ (region: HasRegion[X]): Region[X] = this.region ^ region.region
  def rayFrom(source: HasRegionI, inclusive: Boolean = false)(using X =:= Int): Ray =
    region.rayFrom(source, inclusive)
  def rayFromHere(using source: HasVecI)(using X =:= Int): Ray =
    Ray.from(source, asVecI.region)
  
  // BASIC OPERATIONS
  
  /** The dimensionality (number of components) of this vector. */
  def dim: Int
  
  /** The n-th component of this vector in the standard basis. */
  def apply(axis: Int): X
  
  /** All components of this vector in the standard basis. */
  def components: IndexedSeq[X]
  
  /** An iterator over all components of this vector in the standard basis. */
  def iterator: Iterator[X] = components.iterator
  
  /** Transform each of the components of this vector independently. */
  def map[Y : Ring : Ordering](f: X => Y): Vec[Y] = Vec(components.map(f))
  
  /** Merge two vectors component-wise. */
  def zip[Y : Ring : Ordering, Z : Ring : Ordering](v: Vec[Y])(f: (X, Y) => Z): Vec[Z] =
    Vec(components.zip(v.components).map(f.tupled))
  
  /** Whether this vector is 0-dimensional. */
  def isEmpty: Boolean = dim == 0
  /** Whether this vector is not 0-dimensional. */
  def nonEmpty: Boolean = dim > 0
  /** Whether all components of this vector are zero. */
  def isZero: Boolean = forall(_ == zero)
  
  // INFIX OPERATORS
  
  /** Add two vectors component-wise. */
  def + (that: HasVec[X]): Vec[X] = zip(that.position)(_ + _)
  def + (region: HasRegion[X]): Region[X] = region.region + this
  
  /** Subtract two vectors component-wise. */
  def - (that: HasVec[X]): Vec[X] = this + (-that.position)
  def - (region: HasRegion[X]): Region[X] = (-region.region) + this
  
  /** Multiply each component of this vector by a constant. */
  def * (x: X): Vec[X] = map(_ * x)
  def / (x: X) (using D: Dividable[X]): Vec[X] = map(D.divide(_, x))
  
  /** Multiply two vectors component-wise. */
  def * (v: HasVec[X]): Vec[X] = zip(v.position)(_ * _)
  
  /** Negate each component of this vector. */
  def unary_- : Vec[X] = map(-_)
  
  // ALIASES
  
  /** The first component of this vector; equivalent to `apply(0)`. */
  def x: X = apply(0)
  /** The second component of this vector; equivalent to `apply(1)`. */
  def y: X = apply(1)
  /** The third component of this vector; equivalent to `apply(2)`. */
  def z: X = apply(2)
  
  /** The first component of this vector; equivalent to `apply(0)`. */
  def width: X = apply(0)
  /** The second component of this vector; equivalent to `apply(0)`. */
  def height: X = apply(1)
  /** The third component of this vector; equivalent to `apply(0)`. */
  def depth: X = apply(2)
  
  // INEQUALITIES
  
  /** Whether each component of the first vector is always strictly less than that of the second. */
  def < (v: HasVec[X]): Boolean = zip(v.position)(_ < _).foldLeft(true)(_ & _)
  /** Whether all components of the vector are strictly less than some constant. */
  def < (x: X): Boolean = forall(_ < x)
  /** Whether each component of the first vector is always less than or equal to that of the second. */
  def <= (v: HasVec[X]): Boolean = zip(v.position)(_ <= _).foldLeft(true)(_ & _)
  /** Whether all components of the vector are less than or equal to some constant. */
  def <= (x: X): Boolean = forall(_ <= x)
  /** Whether each component of the first vector is always strictly greater than that of the second. */
  def > (v: HasVec[X]): Boolean = zip(v.position)(_ > _).foldLeft(true)(_ & _)
  /** Whether all components of the vector are strictly greater than some constant. */
  def > (x: X): Boolean = forall(_ > x)
  /** Whether each component of the first vector is always greater than or equal to that of the second. */
  def >= (v: HasVec[X]): Boolean = zip(v.position)(_ >= _).foldLeft(true)(_ & _)
  /** Whether all components of the vector are greater than or equal to some constant. */
  def >= (x: X): Boolean = forall(_ >= x)
  
  /** A transformed version of this vector with the absolute value applied to all components. */
  def abs: Vec[X] = map(x => if x < zero then -x else x)
  
  // GEOMETRIC OPERATIONS
  
  /** The dot product (sum of pair-wise products) between two vectors. */
  infix def dot(v: HasVec[X]): X = zip(v.position)(_ * _).sum
  /** The cross product between two 3D vectors. */
  infix def cross(v: HasVec[X]): Vec[X] = Vec (
    y * v.position.z - z * v.position.y,
    z * v.position.x - x * v.position.z,
    x * v.position.y - y * v.position.x,
  )
  
  /** A transformed version of this vector flipped of the `axis`. */
  def flip(axis: Int): Vec[X] = update(axis, -apply(axis))
  /** A transformed version of this vector flipped over the X-axis. */
  def flipX: Vec[X] = flip(0)
  /** A transformed version of this vector flipped over the Y-axis. */
  def flipY: Vec[X] = flip(1)
  /** A transformed version of this vector flipped over the Z-axis. */
  def flipZ: Vec[X] = flip(2)
  
  /** A transformed version of this vector rotated 90 degrees from the `from`-axis to the `to`-axis. */
  def rotate(from: Int, to: Int): Vec[X] = update(to, apply(from)).update(from, -apply(to))
  /** A transformed version of this vector rotated 90 degrees in the XY-plane from i to j. */
  def rotateXY: Vec[X] = rotate(0, 1)
  /** A transformed version of this vector rotated 90 degrees in the XY-plane from j to i. */
  def rotateYX: Vec[X] = rotate(1, 0)
  /** A transformed version of this vector rotated 90 degrees in the XZ-plane from i to k. */
  def rotateXZ: Vec[X] = rotate(0, 2)
  /** A transformed version of this vector rotated 90 degrees in the XZ-plane from k to i. */
  def rotateZX: Vec[X] = rotate(2, 0)
  /** A transformed version of this vector rotated 90 degrees in the YZ-plane from j to k. */
  def rotateYZ: Vec[X] = rotate(1, 2)
  /** A transformed version of this vector rotated 90 degrees in the YZ-plane from k to j. */
  def rotateZY: Vec[X] = rotate(2, 1)
  
  /** Project this vector onto another. */
  def proj(axis: HasVec[X])(using Metric[X], Dividable[X]): Vec[X] =
    (this dot axis.position.normalise) * axis.position.normalise
    
  /** Project this vector onto the given axis, i.e. replace all other components with 0. */
  def proj(axis: Int): Vec[X] = Vec.zero(dim).update(axis, apply(axis))
  /** Project this vector onto the X-axis, i.e. replace all other components with 0. */
  def projX: Vec[X] = proj(0)
  /** Project this vector onto the Y-axis, i.e. replace all other components with 0. */
  def projY: Vec[X] = proj(1)
  /** Project this vector onto the Z-axis, i.e. replace all other components with 0. */
  def projZ: Vec[X] = proj(2)
  
  // MODIFICATIONS
  
  /** A copy of this vector with the `axis`-th component replaced with `x`. */
  def update(axis: Int, x: X): Vec[X] =
    takeLeft(axis) ++ Vec.zero(axis - dim) ++ Vec(x) ++ dropLeft(axis + 1)
  
  /** Sample the vector at the given `axes`. */
  def swizzle(axes: Int*): Vec[X] = Vec(axes.map(apply))
  
  /** Linearly transform this vector so that the basis vectors are mapped to the given `dirs`. */
  def transform(dirs: HasVec[X]*): Vec[X] =
    iterator.zip(dirs).map(_ * _.position).foldLeft(Vec.empty)(_ + _)
  
  /** Restrict all components of this vector to be non-negative. */
  def clamp(using Ordering[X]): Vec[X] = clamp(zero)
  /** Restrict all components of this vector to be at least `min`. */
  def clamp(min: X = zero)(using Ordering[X]): Vec[X] =
    map(x => if x < min then min else x)
  /** Restrict all components of this vector to be between `min` and `max` inclusive.  */
  def clamp(min: X, max: X)(using Ordering[X]): Vec[X] =
    map(x => if x < min then min else if x > max then max else x)
  
  // LIST OPERATIONS
  
  /** Retain only the components from the `from`-axis (inclusive) to the `to`-axis (exclusive). */
  def slice(from: Int, until: Int): Vec[X] = Vec(components.slice(from, until))
  /** Append the components of 2 vectors end-to-end. */
  def ++ (v: HasVec[X]): Vec[X] = Vec(components ++ v.position.components)
  /** Append an element to the start of this vector. */
  def +: (x: X): Vec[X] = Vec(x +: components)
  /** Append an element to the end of this vector. */
  def :+ (x: X): Vec[X] = Vec(components :+ x)
  
  /** Keep only the first `n` components. */
  def takeLeft(n: Int): Vec[X] = slice(0, n)
  /** Discard the first `n` components. */
  def dropLeft(n: Int): Vec[X] = slice(n, dim)
  /** Keep only the last `n` components. */
  def takeRight(n: Int): Vec[X] = slice(dim - n, dim)
  /** Discard the last `n` components. */
  def dropRight(n: Int): Vec[X] = slice(0, dim - n)
  
  /** A left-to-right list reduction operation. */
  def foldLeft[Y](y: Y)(f: (Y, X) => Y): Y = iterator.foldLeft(y)(f)
  /** A right-to-left list reduction operation. */
  def foldRight[Y](y: Y)(f: (X, Y) => Y): Y = iterator.foldRight(y)(f)
  
  /** Find the sum of all components. */
  def sum: X = foldLeft(zero)(_ + _)
  /** Find the product of all components. */
  def product: X = foldLeft(one)(_ * _)
  
  /** Whether the given `f` is true for all components. True by default if empty. */
  def forall(f: X => Boolean): Boolean = map(f).foldLeft(true)(_ & _)
  /** Whether the given `f` is true for some component. False by default if empty. */
  def exists(f: X => Boolean): Boolean = map(f).foldLeft(false)(_ | _)
  
  infix def in(region: HasRegion[X]): Boolean =
    region.region.contains(this)
    
  def asUnbounded: UVec[X] = map(Finite(_))
  
  def isFinite(using X =:= WithInfinity[?]): Boolean = forall(_.isFinite)
  def isInfinite(using X =:= WithInfinity[?]): Boolean = exists(_.isInfinite)
  
  def asFinite[Y: Ring: Ordering](using X <:< WithInfinity[Y]): Vec[Y] = map(_.asFinite)
  
  override def toString: String = iterator.mkString("[", ", ", "]")
  
  override def equals(that: Any): Boolean = that match
    case v: Vec[?] => components == v.components
    case _ => false
    
  override def hashCode: Int =
    components.foldLeft(0)((hash, x) => hash << 4 + x.hashCode)
  
  /** The additive identity in this ring. */
  private def zero: X = summon[Ring[X]].additiveIdentity
  /** The multiplicative identity in this ring. */
  private def one: X = summon[Ring[X]].multiplicativeIdentity
  
  /** Round each component of this `VecF` down to the next integer. */
  def toVecI(using X =:= Float): VecI =
    map(_.toInt)
  
  /** Represent each component of this `VecI` as a real value. */
  def toVecF(using X =:= Int): VecF =
    map(_.toFloat)
    
  def direction(using X =:= Int): VecI =
    val divisor = gcd(asVecI.abs.components*)
    asVecI.map(_ / divisor)
  
  /** The relative direction between two `Vec`s, divided by the GCD of its components. */
  def directionTo(u: HasVecI)(using X =:= Int): VecI =
    (u.position - asVecI).direction
  
  /** The number of steps it takes to get from `v` to `u` if using `v.directionTo(u)` on each step. */
  def stepsTo(u: HasVecI)(using X =:= Int): Int =
    (u.position - asVecI).asMultipleOf(asVecI.directionTo(u)).get
  
  /** @return If `v` is an integer multiple of `u`, `Some(a)` s.t. `v=au`, otherwise `None`. */
  def asMultipleOf(u: HasVecI)(using X =:= Int): Option[Int] =
    if isZero then Some(0)
    else if components.zip(u.position.components).exists(_ == 0 ^ _ == 0) then None
    else
      val pairs = asVecI.components.filter(_ != 0).zip(u.position.components.filter(_ != 0))
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
  def rayTo(to: HasVecI)(using X =:= Int): Ray =
    Ray.between(asVecI, to)
  
  /**
   * Create a new ray consisting of all points on the line between two `Vec`s, excluding the end.
   * Each step is determined using `VecI.directionTo`.
   */
  def rayUntil(until: HasVecI)(using X =:= Int): Ray =
    rayTo(until).retract(1)
  
  /**
   * Create a ray consisting of all points along some direction(s) from this `Vec`.
   *
   * @param dir The direction(s) in which the ray extends.
   * @param inclusive Whether this vertex is included (default=false).
   */
  def ray(dir: HasRegionI, inclusive: Boolean = false)(using X =:= Int): Ray =
    Ray.from(asVecI, dir, inclusive)
  
  /** The arithmetic mean of two `VecI`s. May produce rounding artifacts. */
  def midpoint(u: HasVecI)(using X =:= Int): VecI = VecI.midpoint(asVecI, u)
  
  private[math] def asVecI(using X =:= Int): VecI =
    this.asInstanceOf[VecI]
    
  private[math] def asUVec[Y: Ring: Ordering](using X =:= WithInfinity[Y]): UVec[Y] =
    this.asInstanceOf[UVec[Y]]
  
  private[math] def asUVecI(using X =:= ExtendedInt): UVecI =
    this.asInstanceOf[UVecI]
  
object Vec:
  
  type VecI = Vec[Int]
  type VecL = Vec[Long]
  type VecF = Vec[Float]
  type VecD = Vec[Double]
  
  type UVec[X] = Vec[WithInfinity[X]]
  type UVecI = Vec[ExtendedInt]
  type UVecL = Vec[ExtendedLong]
  type UVecF = Vec[ExtendedFloat]
  type UVecD = Vec[ExtendedDouble]
  
  /** The canonical `Vec` implementation, backed by an `IndexedSeq`. */
  private class SeqVec[@specialized X : Ordering] (
    val components: IndexedSeq[X],
  ) (using R: Ring[X]) extends Vec[X]:
    def apply(n: Int): X = if components.isDefinedAt(n) then components(n) else R.additiveIdentity
    override val dim: Int = components.size
    
  /** Construct a new `Vec` with the given components in the standard basis. */
  def apply[X : Ring : Ordering](x: X*): Vec[X] = SeqVec(x.toIndexedSeq)
  /** Construct a new `Vec` with the given components in the standard basis. */
  def apply[X : Ring : Ordering](x: Iterable[X]): Vec[X] = SeqVec(x.toIndexedSeq)
  
  /** Construct a `d`-dimensional `Vec` of all 0's, where 0 is the additive identity. */
  def zero[X : Ordering](d: Int = 0)(using R: Ring[X]): Vec[X] =
    Vec(Seq.fill(d)(R.additiveIdentity))
  /** The unique 0-dimensional `Vec` over the ring `X`. */
  def empty[X : Ring : Ordering]: Vec[X] = zero(0)
  
  /** Construct a `d`-dimensional `Vec` of all 1's, where 1 is the multiplicative identity. */
  def one[X : Ordering](d: Int = 0)(using R: Ring[X]): Vec[X] =
    Vec(Seq.fill(d)(R.multiplicativeIdentity))
  
  /**
   * Construct a `d`-dimensional `Vec` of all 0's except in the `n`-th component, where there is a 1.
   * If `d` < `n`, then the value of `d` given is ignored and taken to be `n` instead.
   */
  def axis[X : Ordering](n: Int, d: Int = 0)(using R: Ring[X]): Vec[X] =
    Vec:
      Seq.fill(n)(R.additiveIdentity) :+
      R.multiplicativeIdentity :++
      Seq.fill(d - n - 1)(R.additiveIdentity)
  
  /** Construct a `d`-dimensional `Vec` where all components have the same, given value. */
  def fill[X : Ring : Ordering](d: Int)(x: X): Vec[X] = Vec(Seq.fill(d)(x))
    
  // If a metric is defined for this vector space:
  extension [X : Ring : Ordering] (v: Vec[X]) (using M: Metric[X])
    
    /** The length of this vector according to the current `Metric`. */
    def norm: X = M.norm(v)
    
    /** Normalise this vector to have length 1 according to the current `Metric`. */
    def normalise(using Dividable[X]): Vec[X] = v / v.norm
    
    /** The distance between two vectors. */
    def dist(u: Vec[X]): X = M.dist(v, u)
    
  // If this vector space has some notion of adjacency:
  extension [X : Ring : Ordering] (v: Vec[X]) (using M: EnumerableMetric[X])
    
    /** Whether these vectors are adjacent. */
    def adjacent(u: Vec[X]): Boolean = M.adjacent(v, u)
    
    /** Enumerate all vectors which are adjacent to this one. */
    //def neighbours: Region[X] = M.neighbours(v)
  
  object VecI:
    
    /** Construct a new `VecI` with the given components in the standard basis. */
    def apply(A: Int*): VecI = Vec[Int](A)
    /** Construct a new `VecI` with the given components in the standard basis. */
    def apply(A: Iterable[Int]): VecI = Vec[Int](A)
    
    /** Construct a `d`-dimensional `VecI` of all 0's. */
    def zero(d: Int = 0): VecI = Vec.zero[Int](d)
    
    /** The unique 0-dimensional `VecI`. */
    def empty: VecI = Vec.empty[Int]
    
    /** Construct a `d`-dimensional `VecI` of all 1's. */
    def one(d: Int = 0): VecI = Vec.one[Int](d)
    
    /**
     * Construct a `d`-dimensional `VecI` of all 0's except in the `n`-th component, where there is a 1.
     * If `d` < `n`, then the value of `d` given is ignored and taken to be `n` instead.
     */
    def axis(n: Int, d: Int = 0): VecI = Vec.axis[Int](n, d)
    
    /** Construct a `d`-dimensional `VecI` where all components have the same, given value. */
    def fill(d: Int)(x: Int): VecI = Vec.fill(d)(x)
    
    /** The arithmetic mean of the given `VecI`s. May produce rounding artifacts. */
    def midpoint(v: HasVecI*): VecI = v.map(_.position).reduce(_ + _) / v.size
  
  // If `X` has an `Ordering`, then so does `Vec[X]`.
  given [X : Ring : Ordering]: Ordering[Vec[X]] with
    def compare(x: Vec[X], y: Vec[X]): Int = ((x.components, y.components): @unchecked) match
      case (x0 +: _, y0 +: _) if x0.compare(y0) != 0 => x0.compare(y0)
      case (x0 +: xs, y0 +: ys) if x0.compare(y0) == 0 => xs.compare(ys)
      case _ if x.isEmpty && y.isEmpty => 0
      case _ if x.isEmpty => -1
      case _ if y.isEmpty => 1
      
  // `Vec[X]` forms a `Group` for any `Ring` `X`.
  given [X : Ring : Ordering]: Group[Vec[X]] with
    def sum(x: Vec[X], y: Vec[X]): Vec[X] = x + y
    def additiveIdentity: Vec[X] = Vec.empty[X]
    def additiveInverse(x: Vec[X]): Vec[X] = -x
  
  // By default, `VecF` should use the Euclidean metric.
  given Metric[Float] = Metric.Euclidean
  
  /** Proves that it is possible to divide two elements of this type. */
  trait Dividable[X]:
    def divide(x: X, y: X): X
  // Any `Field` can be divided using the multiplicative inverse.
  given [X] (using F: Field[X]): Dividable[X] with
    def divide(x: X, y: X): X = x * F.multiplicativeInverse(y)
  // `Int`s can be divided by rounding the result.
  given Dividable[Int] with
    def divide(x: Int, y: Int): Int = x / y
  
  // In general, it is permissible to use `VecI` as a substitute for `VecF`.
  given Conversion[VecI, VecF] = _.toVecF
  
  trait HasVec[X : Ring : Ordering] extends HasRegion[X]:
    val position: Vec[X]
    def region: Region[X] = Region.point(position)
    
  type HasVecI = HasVec[Int]
  
  type HasUVec[X] = HasVec[WithInfinity[X]]
  type HasUVecI = HasUVec[Int]