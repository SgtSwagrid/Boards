package util.math

import util.math.Algebra.{*, given}
import util.math.Number.gcd
import util.math.kernel.{Dir, Kernel, Ray}

import io.circe.{Encoder, Decoder}

import scala.annotation.targetName
import scala.math.Ordered.orderingToOrdered

case class Vec [@specialized X: Ring] (private val A: X*):
  
  import Vec.*
  
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
  
  inline def dot(v: Vec[X]): X = Vec(A.zip(v.A).map(_ * _)*).sum
  inline def cross(v: Vec[X]): Vec[X] =
    Vec(y * v.z - z * v.y, z * v.x - x * v.z, x * v.y - y * v.x)
  
  inline def norm(using M: Metric)(using X =:= Int): Int =
    M.norm(this.asInstanceOf[VecI])
    
  inline def ball
    (rmax: Int, rmin: Int = 0)
    (using M: Metric)
    (using X =:= Int)
  : Kernel[?] =
    M.ball(thisAsVecI, rmax, rmin)
    
  inline def dist(that: VecI)(using M: Metric)(using X =:= Int): Int =
    M.dist(thisAsVecI, that)
  inline def adjacent(that: VecI)(using M: Metric)(using X =:= Int): Boolean =
    M.adjacent(thisAsVecI, that)
  inline def neighbours(using M: Metric)(using X =:= Int): Kernel[?] =
    M.neighbours(thisAsVecI)
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
    
  inline def rotate(from: Int, to: Int): Vec[X] =
    update(to, this(from)).update(from, -this(to))
    
  inline def absolute(using X =:= Int): VecI = thisAsVecI.map(_.abs)
  
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
  
  private inline def zero: X = summon[Ring[X]].additiveIdentity
  private inline def one: X = summon[Ring[X]].multiplicativeIdentity
  private inline def thisAsVecI(using X =:= Int): VecI = this.asInstanceOf[VecI]
    
object Vec:
  
  inline def zero[X: Ring]: Vec[X] = zero(0)
  inline def zero[X](d: Int = 0)(using R: Ring[X]): Vec[X] =
    Vec(Seq.fill(d)(R.additiveIdentity)*)
  
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
    
  trait Dividable[X]:
    def divide(x: X, y: X): X
  given [X: Field]: Dividable[X] with
    def divide(x: X, y: X): X = x / y
  given Dividable[Int] with
    def divide(x: Int, y: Int): Int = x / y
  
  given [X: Encoder]: Encoder[Vec[X]] = Encoder.encodeSeq[X].contramap(_.toSeq)
  given [X: Decoder: Ring]: Decoder[Vec[X]] = Decoder.decodeSeq[X].map(Vec.apply)