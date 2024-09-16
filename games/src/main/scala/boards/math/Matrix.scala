package boards.math

import Algebra.{*, given}
import Vec.{*, given}

import scala.annotation.targetName

case class Matrix [@specialized X] private
  (private val A: Vec[X]*)
  (using R: Ring[X])
derives CanEqual:
  
  import Matrix.*
  
  inline def apply(i: Int, j: Int, default: X = zero): X =
    assert(inBounds(i, j))
    A(j)(i)
    
  @targetName("add") inline def + (m: Matrix[X]): Matrix[X] = zip(m)(_ + _)
  @targetName("subtract") inline def - (m: Matrix[X]): Matrix[X] = this + -m
  @targetName("multiply") inline def * (x: X): Matrix[X] = map(R.product(_, x))
  @targetName("multiply") inline def * (v: Vec[X]): Vec[X] =
    cols.zip(v.toSeq).map(_ * _).reduce(_ + _)
  @targetName("multiply") inline def * [M <: Matrix[X]] (m: Matrix[X]): Matrix[X] =
    Matrix(m.cols.map(this * _)*)
  @targetName("negate") inline def unary_- : Matrix[X] = map(-_)
  
  inline def cols: Seq[Vec[X]] = A
  inline def rows: Seq[Vec[X]] = (0 until height).map: i =>
    Vec((0 until width).map(j => A(j)(i))*)
  inline def transpose: Matrix[X] = Matrix(rows*)
  
  inline def map[Y: Ring](f: X => Y): Matrix[Y] = Matrix(A.map(_.map(f)) *)
  inline def zip[Y: Ring, Z: Ring](m: Matrix[Y])(f: (X, Y) => Z): Matrix[Z] =
    assert(height == m.height && width == m.width)
    Matrix(A.zipAll(m.A, Vec.zero[X], Vec.zero[Y]).map(_.zip(_)(f)) *)
  
  inline def update(i: Int, j: Int, x: X): Matrix[X] =
    assert(inBounds(i, j))
    Matrix(A.updated(j, A(j).update(i, x)) *)
  inline def updateCol(j: Int, v: Vec[X]): Matrix[X] =
    assert(inBounds(0, j) && v.dim == height)
    Matrix(A.updated(j, v)*)
  inline def updateRow(i: Int, v: Vec[X]): Matrix[X] =
    transpose.updateCol(i, v).transpose
  
  val height: Int = A.map(_.dim).max
  val width: Int = A.length
  inline def inBounds(i: Int, j: Int): Boolean =
    i >= 0 && i < height && j >= 0 && j < width
  
  private inline def zero: X = R.additiveIdentity
  private inline def one: X = R.multiplicativeIdentity
  
object Matrix:
  
  class InvertibleMatrix[@specialized X: Ring]
    (private val A: Vec[X]*)
    (private val A_inv: Vec[X]*)
  extends Matrix[X](A*):
    
    type Product[M <: Matrix[X]] <: Matrix[X] = M match
      case InvertibleMatrix[X] => InvertibleMatrix[X]
      case Matrix[X] => Matrix[X]
    
    @targetName("multiply") override def * [M <: Matrix[X]] (m: M): Product[M] =
      m match
        case m: InvertibleMatrix[X] =>
          InvertibleMatrix((Matrix(A*) * Matrix(m.A*)).A*)((Matrix(m.A_inv*) * Matrix(A_inv*)).A*)
        case m: Matrix[X] => super.*(m)
        
    def inverse: InvertibleMatrix[X] =
      InvertibleMatrix(A_inv*)(A*)
  
  inline def zero[X](h: Int, w: Int)(using R: Ring[X]): Matrix[X] =
    Matrix(Seq.fill(h)(Vec.zero(w))*)
  
  inline def one[X](h: Int, w: Int)(using R: Ring[X]): Matrix[X] =
    Matrix(Seq.fill(h)(Vec.one(w))*)
  
  inline def identity[X](dim: Int)(using R: Ring[X]): InvertibleMatrix[X] =
    val cols = (0 until dim).map(Vec.axis(_, dim))
    InvertibleMatrix(cols*)(cols*)
    
  inline def rotate[X: Ring](from: Int, to: Int, dim: Int): InvertibleMatrix[X] =
    val cols = (0 until dim).map(Vec.axis(_, dim))
    InvertibleMatrix
      (cols.updated(from, Vec.axis(to ,dim)).updated(to, -Vec.axis(from))*)
      (cols.updated(to, Vec.axis(from, dim)).updated(from, -Vec.axis(to))*)
    
  inline def flip[X](axis: Int, dim: Int)(using R: Ring[X]): InvertibleMatrix[X] =
    val cols = (0 until dim).map(i => Vec.axis(i, dim) * R.sign(i != axis))
    InvertibleMatrix(cols*)(cols*)
  
  inline def cols[X: Ring](v: Vec[X]*): Matrix[X] = Matrix(v*)
  inline def rows[X: Ring](v: Vec[X]*): Matrix[X] = Matrix(v*).transpose