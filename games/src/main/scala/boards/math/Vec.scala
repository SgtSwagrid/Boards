package boards.math

import Algebra.{*, given}
import Metric.*
import Number.gcd
import boards.math.kernel.{Dir, Kernel, Ray}

import scala.annotation.targetName
import scala.compiletime.erasedValue
import scala.math.Ordered.orderingToOrdered

case class Vec [@specialized X: Ring] (private val A: X*):
  
  import Vec.{*, given}
  
  inline def apply(i: Int, default: X = zero): X =
    if 0 <= i && i < A.length then A(i)
    else if -A.length <= i && i < 0 then A(i + A.length)
    else default
  
  @targetName("add")
  inline def + (v: Vec[X]): Vec[X] = zip(v)(_ + _)
  @targetName("subtract")
  inline def - (v: Vec[X]): Vec[X] = this + -v
  @targetName("multiply")
  inline def * (x: X): Vec[X] = map(_ * x)
  @targetName("multiply")
  inline def * (v: Vec[X]): Vec[X] = zip(v)(_ * _)
  @targetName("multiply")
  inline def * (m: Matrix[X]): Vec[X] = m.transpose * this
  @targetName("divide")
  inline def / (d: X)(using D: Dividable[X]): Vec[X] = map(x => D.divide(x, d))
  @targetName("negate")
  inline def unary_- : Vec[X] = map(-_)
  
  @targetName("translate")
  inline def + (dir: Dir)(using X =:= Int): Kernel[?] =
    dir + thisAsVecI
  @targetName("translate")
  inline def + [Y] (kernel: Kernel[Y])(using X =:= Int): Kernel[Y] =
    kernel.translate(thisAsVecI)
  
  @targetName("lessThan")
  inline def < (that: Vec[X])(using Ordering[X]): Boolean =
    this.zip(that)(_ < _).foldLeft(true)(_ & _)
  @targetName("lessThanEqual")
  inline def <= (that: Vec[X])(using Ordering[X]): Boolean =
    this.zip(that)(_ <= _).foldLeft(true)(_ & _)
  @targetName("greaterThan")
  inline def > (that: Vec[X])(using Ordering[X]): Boolean =
    this.zip(that)(_ > _).foldLeft(true)(_ & _)
  @targetName("greaterThanEqual")
  inline def >= (that: Vec[X])(using Ordering[X]): Boolean =
    this.zip(that)(_ >= _).foldLeft(true)(_ & _)
  
  inline def dim: Int = A.length
  inline def isEmpty: Boolean = dim == 0
  inline def isZero: Boolean = A.forall(_ == zero)
  
  inline def x: X = apply(0)
  inline def y: X = apply(1)
  inline def z: X = apply(2)
  
  inline def width: X = apply(0)
  inline def height: X = apply(1)
  inline def depth: X = apply(2)
  
  inline infix def dot(v: Vec[X]): X = Vec(A.zip(v.A).map(_ * _)*).sum
  inline infix def cross(v: Vec[X]): Vec[X] =
    Vec(y * v.z - z * v.y, z * v.x - x * v.z, x * v.y - y * v.x)
  
  inline def norm(using M: Metric[X]): X =
    M.norm(this)
    
  inline def normalise(using Metric[X], Dividable[X]): Vec[X] =
    this / this.norm
    
  inline def ball
    (rmax: Int, rmin: Int = 0)
    (using M: EnumerableMetric[X])
    (using X =:= Int)
  : Kernel[?] =
    M.ball(this, rmax, rmin)
    
  inline def dist(that: Vec[X])(using M: Metric[X]): X =
    M.dist(this, that)
  inline def adjacent(that: Vec[X])(using M: EnumerableMetric[X]): Boolean =
    M.adjacent(this, that)
  inline def neighbours(using M: EnumerableMetric[X]): Kernel[?] =
    M.neighbours(this)
  inline def neighbours(direction: Dir)(using X =:= Int): Kernel[?] =
    direction.from(thisAsVecI)
  
  inline def directionTo(that: VecI)(using X =:= Int): VecI =
    (that - thisAsVecI) / gcd((that - thisAsVecI).absolute.toSeq*)
  inline def midpoint(that: VecI)(using X =:= Int): VecI =
    VecI.midpoint(thisAsVecI, that)
  
  inline def ray
    (direction: Dir, inclusive: Boolean = false)
    (using domain: Kernel[?] = Kernel.infinite)
    (using X =:= Int)
  : Ray =
    Ray.from(thisAsVecI, direction, inclusive)
  
  inline def rayTo(that: VecI)(using X =:= Int): Ray = Ray.between(thisAsVecI, that)
  inline def rayUntil(that: VecI)(using X =:= Int): Ray = Ray.between(thisAsVecI, that).drop(1) //wrong, drops from wrong end!
    
  inline def map[Y: Ring](f: X => Y): Vec[Y] = Vec(A.map(f)*)
  inline def zip[Y, Z: Ring](v: Vec[Y])(f: (X, Y) => Z): Vec[Z] =
    Vec(A.zipAll(v.A, zero, v.zero).map(f.tupled)*)
  
  inline def update(axis: Int, x: X): Vec[X] =
    if axis >= dim then this ++ Vec.zero[X](axis - dim) :+ x
    else Vec(A.updated(axis, x)*)
    
  inline def swizzle(i: Int*): Vec[X] = Vec(i.map(apply(_))*)
  inline def slice(from: Int, until: Int = dim): Vec[X] = Vec(A.slice(from, until)*)
  
  inline def transform(axes: Vec[X]*): Vec[X] =
    A.zip(axes).map(_ * _).foldLeft(Vec.zero)(_ + _)
  
  inline def flip(axis: Int): Vec[X] =
    update(axis, -this(axis))
  inline def flipX: Vec[X] = flip(0)
  inline def flipY: Vec[X] = flip(1)
  inline def flipZ: Vec[X] = flip(2)
    
  inline def rotate(from: Int, to: Int): Vec[X] =
    update(to, this(from)).update(from, -this(to))
  inline def rotateXY: Vec[X] = rotate(0, 1)
  inline def rotateYX: Vec[X] = rotate(1, 0)
  inline def rotateXZ: Vec[X] = rotate(0, 2)
  inline def rotateZX: Vec[X] = rotate(2, 0)
  inline def rotateYZ: Vec[X] = rotate(1, 2)
  inline def rotateZY: Vec[X] = rotate(2, 1)
  
  inline def proj(axis: Vec[X])(using Metric[X], Dividable[X]): Vec[X] =
    (this dot axis.normalise) * axis.normalise
  inline def proj(axis: Int): Vec[X] =
    Vec.zero[X](dim).update(axis, apply(axis))
  inline def projX: Vec[X] = proj(0)
  inline def projY: Vec[X] = proj(1)
  inline def projZ: Vec[X] = proj(2)
  
  inline def clamp(using Ordering[X]): Vec[X] = clamp(zero)
  
  inline def clamp(min: X = zero)(using Ordering[X]): Vec[X] =
    map:
      case x if x < min => min
      case x => x
  
  inline def clamp(min: X, max: X)(using Ordering[X]): Vec[X] =
    map:
      case x if x < min => min
      case x if x > max => max
      case x => x
    
  inline def absolute(using S: Signed[X]): Vec[X] = map(S.abs)
  
  inline def toVecF(using X =:= Int): VecF = thisAsVecI.map(_.toFloat)
  inline def toVecI(using X =:= Float): VecI = thisAsVecF.map(_.toInt)
  
  inline def first: X = apply(0)
  inline def last: X = apply(dim - 1)
  
  inline def takeLeft(n: Int): Vec[X] = Vec(A.take(n)*)
  inline def dropLeft(n: Int): Vec[X] = Vec(A.drop(n)*)
  inline def takeRight(n: Int): Vec[X] = Vec(A.takeRight(n)*)
  inline def dropRight(n: Int): Vec[X] = Vec(A.dropRight(n)*)
  
  @targetName("append") inline def :+ (x: X): Vec[X] = Vec(A :+ x *)
  @targetName("prepend") inline def +: (x: X): Vec[X] = Vec(x +: A *)
  @targetName("concatenate") inline def ++ (v: Vec[X]): Vec[X] = Vec(A ++ v.A *)
  
  inline def foldLeft[Y](y: Y)(f: (Y, X) => Y): Y = A.foldLeft(y)(f)
  inline def foldRight[Y](y: Y)(f: (X, Y) => Y): Y = A.foldRight(y)(f)
  
  inline def sum: X = foldLeft(zero)(_ + _)
  inline def product: X = foldLeft(one)(_ * _)
  
  inline def left(n: X): Vec[X] = this + (Vec.left * n)
  inline def right(n: X): Vec[X] = this + (Vec.right * n)
  inline def up(n: X): Vec[X] = this + (Vec.up * n)
  inline def down(n: X): Vec[X] = this + (Vec.down * n)
  
  inline def toSeq: Seq[X] = A
  
  inline override def toString: String = A.mkString("[", ", ", "]")
  
  inline override def equals(that: Any): Boolean =
    that match
      case that: Vec[?] => A == that.A
      case _ => false
  
  private inline def zero: X = summon[Ring[X]].additiveIdentity
  private inline def one: X = summon[Ring[X]].multiplicativeIdentity
  private inline def thisAsVecI(using X =:= Int): VecI = this.asInstanceOf[VecI]
  private inline def thisAsVecF(using X =:= Float): VecF = this.asInstanceOf[VecF]
    
object Vec:
  
  inline def zero[X](d: Int = 0)(using R: Ring[X]): Vec[X] =
    Vec(Seq.fill(d)(R.additiveIdentity) *)
  inline def zero[X: Ring]: Vec[X] = zero(0)
  
  inline def one[X](d: Int = 0)(using R: Ring[X]): Vec[X] =
    Vec(Seq.fill(d)(R.multiplicativeIdentity)*)
  
  inline def axis[X](n: Int, d: Int = 0)(using R: Ring[X]): Vec[X] =
    Vec(
      Seq.fill(n)(R.additiveIdentity) :+
      R.multiplicativeIdentity :++
      Seq.fill(d - n - 1)(R.additiveIdentity)
    *)
  
  inline def left[X](using R: Ring[X]): Vec[X] =
    Vec(-R.multiplicativeIdentity, R.additiveIdentity)
  inline def right[X](using R: Ring[X]): Vec[X] =
    Vec(R.multiplicativeIdentity, R.additiveIdentity)
  inline def up[X](using R: Ring[X]): Vec[X] =
    Vec(R.additiveIdentity, R.multiplicativeIdentity)
  inline def down[X](using R: Ring[X]): Vec[X] =
    Vec(R.additiveIdentity, -R.multiplicativeIdentity)
    
  inline def fill[X: Ring](d: Int)(x: X): Vec[X] = Vec(Seq.fill(d)(x)*)
    
  type VecI = Vec[Int]
  type VecF = Vec[Float]
  
  object VecI:
    
    inline def apply(A: Int*): VecI = Vec[Int](A*)
    
    inline def zero: VecI = Vec.zero[Int]
    inline def zero(d: Int = 0): VecI = Vec.zero[Int](d)
    inline def one(d: Int = 0): VecI = Vec.one[Int](d)
    inline def axis(n: Int, d: Int = 0): VecI = Vec.axis[Int](n, d)
    
    inline def left: VecI = Vec.left[Int]
    inline def right: VecI = Vec.right[Int]
    inline def up: VecI = Vec.up[Int]
    inline def down: VecI = Vec.down[Int]
    
    inline def fill(d: Int)(x: Int): VecI = Vec.fill(d)(x)
    
    inline def midpoint(v: VecI*): VecI = v.reduce(_ + _) / v.size
  
  /*@targetName("prepend") object +: :
    def unapply[X: Ring](v: Vec[X]): Option[(X, Vec[X])] =
      if v.isEmpty then None else Some(v.first, v.dropLeft(1))
  
  @targetName("append") object :+ :
    def unapply[X: Ring](v: Vec[X]): Option[(Vec[X], X)] =
      if v.isEmpty then None else Some(v.dropRight(1), v.last)*/
  
  given [X: Ring: Ordering]: Ordering[Vec[X]] with
    inline def compare(x: Vec[X], y: Vec[X]): Int = ((x.toSeq, y.toSeq): @unchecked) match
      case (x0 +: _, y0 +: _) if x0.compare(y0) != 0 => x0.compare(y0)
      case (x0 +: xs, y0 +: ys) if x0.compare(y0) == 0 => xs.compare(ys)
      case _ if x.isEmpty && y.isEmpty => 0
      case _ if x.isEmpty => -1
      case _ if y.isEmpty => 1
      
  given [X: Ring]: Group[Vec[X]] with
    inline def sum(x: Vec[X], y: Vec[X]): Vec[X] = x + y
    inline def additiveIdentity: Vec[X] = Vec.zero[X]
    inline def additiveInverse(x: Vec[X]): Vec[X] = -x
  
  given Metric[Float] = Metric.Euclidean
  
  trait Dividable[X]:
    def divide(x: X, y: X): X
  given [X: Field]: Dividable[X] with
    def divide(x: X, y: X): X = x / y
  given Dividable[Int] with
    def divide(x: Int, y: Int): Int = x / y
    
  trait Signed[X]:
    def abs(x: X): X
  given Signed[Int] with
    def abs(x: Int): Int = x.abs
  given Signed[Float] with
    def abs(x: Float): Float = x.abs
  
  given Conversion[VecI, VecF] with
    def apply(v: VecI): VecF = v.toVecF