package util.math

import util.math.Algebra.{*, given}

import scala.annotation.targetName
import scala.math.Ordered.orderingToOrdered

case class Vec [@specialized X] (private val A: X*) (using R: Ring[X]) derives CanEqual:
  
  import Vec.*
  
  inline def apply(i: Int, default: X = zero): X =
    if 0 <= i && i < A.length then A(i)
    else if -A.length <= i && i < 0 then A(i + A.length)
    else default
  
  @targetName("add") inline def + (v: Vec[X]): Vec[X] = zip(v)(_ + _)
  @targetName("subtract") inline def - (v: Vec[X]): Vec[X] = this + -v
  @targetName("multiply") inline def * (x: X): Vec[X] = map(_ * x)
  @targetName("multiply") inline def * (v: Vec[X]): Vec[X] = zip(v)(_ * _)
  @targetName("multiply") inline def * (m: Matrix[X]): Vec[X] = m.transpose * this
  @targetName("negate") inline def unary_- : Vec[X] = map(-_)
  
  inline def dim: Int = A.length
  inline def isEmpty: Boolean = dim == 0
  inline def isZero: Boolean = A.forall(_ == zero)
  inline def x: X = apply(0); inline def y: X = apply(1); inline def z: X = apply(2)
  
  inline def dot(v: Vec[X]): X = Vec(A.zip(v.A).map(_ * _)*).sum
  inline def cross(v: Vec[X]): Vec[X] =
    Vec(y * v.z - z * v.y, z * v.x - x * v.z, x * v.y - y * v.x)
    
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
  
  def left(n: X): Vec[X] = this + (Vec.left * n)
  def right(n: X): Vec[X] = this + (Vec.right * n)
  def up(n: X): Vec[X] = this + (Vec.up * n)
  def down(n: X): Vec[X] = this + (Vec.down * n)
  
  inline def toSeq: Seq[X] = A
  
  inline override def toString: String = A.mkString("[", ", ", "]")
  
  private inline def zero: X = R.additiveIdentity
  private inline def one: X = R.multiplicativeIdentity
    
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
  
  inline def left[X](using R: Ring[X]): Vec[X] = Vec(-R.multiplicativeIdentity, R.additiveIdentity)
  inline def right[X](using R: Ring[X]): Vec[X] = Vec(R.multiplicativeIdentity, R.additiveIdentity)
  inline def up[X](using R: Ring[X]): Vec[X] = Vec(R.additiveIdentity, R.multiplicativeIdentity)
  inline def down[X](using R: Ring[X]): Vec[X] = Vec(R.additiveIdentity, -R.multiplicativeIdentity)
  
  extension [X: Field](v: Vec[X])
    @targetName("divide") inline def / (x: X): Vec[X] = v.map(_ * x.inverse)
    
  extension [X: Ordering] (u: Vec[X])
    @targetName("lessThan") inline def < (v: Vec[X]): Boolean =
      u.zip(v)(_ < _).foldLeft(true)(_ & _)
    @targetName("lessThanEqual") inline def <= (v: Vec[X]): Boolean =
      u.zip(v)(_ <= _).foldLeft(true)(_ & _)
    @targetName("greaterThan") inline def > (v: Vec[X]): Boolean =
      u.zip(v)(_ > _).foldLeft(true)(_ & _)
    @targetName("greaterThanEqual") inline def >= (v: Vec[X]): Boolean =
      u.zip(v)(_ >= _).foldLeft(true)(_ & _)
  
  @targetName("prepend") object +: :
    def unapply[X: Ring](v: Vec[X]): Option[(X, Vec[X])] =
      if v.isEmpty then None else Some(v.first, v.dropLeft(1))
  
  @targetName("append") object :+ :
    def unapply[X: Ring](v: Vec[X]): Option[(Vec[X], X)] =
      if v.isEmpty then None else Some(v.dropRight(1), v.last)
      
  given [X: Ring: Ordering]: Ordering[Vec[X]] with
    inline def compare(x: Vec[X], y: Vec[X]): Int = ((x, y): @unchecked) match
      case (x0 +: _, y0 +: _) if x0.compare(y0) != 0 => x0.compare(y0)
      case (x0 +: xs, y0 +: ys) if x0.compare(y0) == 0 => xs.compare(ys)
      case _ if x.isEmpty && y.isEmpty => 0
      case _ if x.isEmpty => -1
      case _ if y.isEmpty => 1
      
  given [X: Ring]: Group[Vec[X]] with
    inline def sum(x: Vec[X], y: Vec[X]): Vec[X] = x + y
    inline def additiveIdentity: Vec[X] = Vec.zero[X]
    inline def additiveInverse(x: Vec[X]): Vec[X] = -x