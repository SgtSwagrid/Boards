package boards.math.ops

import boards.math.algebra.Algebra.{*, given}
import boards.math.algebra.Bijection.AffineBijection
import boards.math.ops.TransformOps.{UScalarFunctor, AffineFunctor}
import boards.math.vector.Vec

object AffineOps:
  
  trait Shift [X: Numeric, +This <: Shift[X, This]]:
    
    def shift (offset: X): This
    
    final inline infix def + (offset: X): This = shift(offset)
    final inline infix def - (offset: X): This = shift(-offset)
  
  trait Translate [X: Numeric, +This <: Translate[X, This]]:
    
    def translate (offset: Vec[X]): This
    
    final inline def translate (offset: X*): This = translate(Vec(offset))
    final inline def fromHere (using offset: Vec[X]): This = translate(offset)
    
    final inline def translateX (offset: X): This = translate(Vec.axis(0, 0) * offset)
    final inline def translateY (offset: X): This = translate(Vec.axis(1, 1) * offset)
    final inline def translateZ (offset: X): This = translate(Vec.axis(2, 2) * offset)
    
    infix def + (offset: Vec[X]): This = translate(offset)
    infix def - (offset: Vec[X]): This = translate(-offset)
  
  trait Flip [+This <: Flip[This]]:
    
    def flip (axis: Int): This
    
    final inline def flipX: This = flip(0)
    final inline def flipY: This = flip(1)
    final inline def flipZ: This = flip(2)
    
  trait Rotate [+This <: Rotate[This]]:
    
    def rotate (from: Int, to: Int): This
    
    final inline def rotateXY: This = rotate(0, 1)
    final inline def rotateYX: This = rotate(1, 0)
    final inline def rotateYZ: This = rotate(1, 2)
    final inline def rotateZY: This = rotate(2, 1)
    final inline def rotateXZ: This = rotate(0, 2)
    final inline def rotateZX: This = rotate(2, 0)
    
  trait ScaleHomogenous [X: Numeric, +This <: ScaleHomogenous[X, This]]:
    
    def scale (factor: X): This
    
    infix def * (factor: X): This = scale(factor)
    infix def / (factor: X) (using Field[X]): This = scale(factor.inverse)
    
  trait ScaleHeterogeneous [X: Numeric as R, +This <: ScaleHeterogeneous[X, This]]
  extends ScaleHomogenous[X, This]:
    
    def scale (factors: Vec[X]): This
    
    final inline def scale (factor: X): This = scale(Vec.fillForever(factor))
    
    final inline infix def * (factors: Vec[X]): This = scale(factors)
    final inline infix def / (factors: Vec[X]) (using Field[X]): This = scale(factors.map(_.inverse))
    
    final inline def scaleX (factor: X): This = scale(Vec(factor).withDefault(R.one))
    final inline def scaleY (factor: X): This = scale(Vec(R.one, factor).withDefault(R.one))
    final inline def scaleZ (factor: X): This = scale(Vec(R.one, R.one, factor).withDefault(R.one))
  
  trait Scalar [X: Numeric, +This <: Scalar[X, This]]
  extends Shift[X, This], ScaleHomogenous[X, This]
  
  /*trait ScalarWithMap [X: Numeric, This[Y] <: ScalarWithMap[Y, This]] extends Scalar[X, This]:
    
    def map [Y: Numeric] (f: X => Y): This[Y]
    
    def unary_- : This[X] = map(-_)
    def + (offset: X): This[X] = map(_ + offset)
    def * (factor: X): This[X] = map(_ * factor)*/
  
  trait Affine [X: Numeric, +This <: Affine[X, This]]
  extends Translate[X, This], Flip[This], Rotate[This], ScaleHeterogeneous[X, This]
  
  /*trait AffineWithMap [X: Numeric, This[Y] <: AffineWithMap[Y, This]] extends Affine[X, This]:
    
    def map [Y: Numeric] (f: Vec[X] => Vec[Y]): This[Y]
    def mapComponents [Y: Numeric] (f: X => Y): This[Y] = map(_.map(f))
    
    def unary_- : This[X] = mapComponents(-_)
    def + (offset: Vec[X]): This[X] = map(_.translate(offset))
    def flip (axis: Int): This[X] = map(_.flip(axis))
    def rotate (from: Int, to: Int): This[X] = map(_.rotate(from, to))
    def * (factor: X): This[X] = map(_ * factor)
    def * (factors: Vec[X]): This[X] = map(_.scale(factors))
    
  trait AffineWithBimap [X: OrderedField, This[Y] <: AffineWithBimap[Y, This]] extends Affine[X, This]:
    
    def bimap [Y: OrderedField] (f: Vec[X] => Vec[Y]) (g: Vec[Y] => Vec[X]): This[Y]
    def bimapComponents [Y: OrderedField] (f: X => Y) (g: Y => X): This[Y] = bimap(_.map(f))(_.map(g))
    
    def unary_- : This[X] = bimap(-_)(-_)
    def + (offset: Vec[X]): This[X] = bimap(_.translate(offset))(_.translate(-offset))
    def flip (axis: Int): This[X] = bimap(_.flip(axis))(_.flip(axis))
    def rotate (from: Int, to: Int): This[X] = bimap(_.rotate(from, to))(_.rotate(to, from))
    def * (factor: X): This[X] = bimap(_ * factor)(_ * factor.inverse)
    def * (factors: Vec[X]): This[X] = bimap(_.scale(factors))(_.scale(factors.map(_.inverse)))
    
  trait ScalarFromMap [X: Numeric, This[Y] <: ScalarFromMap[Y, This]]
  extends Scalar[X, This], UScalarFunctor[X, This]:
    
    def shift (offset: X): This[X] = map(_ + offset)
    def scale (factor: X): This[X] = map(_ * factor)
  
  trait AffineFromMap [X: OrderedField, This[Y] <: ScalarFromMap[Y, This]]
  extends Affine[X, This], AffineFunctor[X, This]:
    
    def translate (offset: Vec[X]): This[X] = mapAffine(AffineBijection.Translate(offset))
    def flip (axis: Int): This[X] = mapAffine(AffineBijection.Flip(axis))
    def rotate (from: Int, to: Int): This[X] = mapAffine(AffineBijection.Rotate(from, to))
    def scale (factor: X): This[X] = mapAffine(AffineBijection.ScaleHomo(factor))
    def scale (factors: Vec[X]): This[X] = mapAffine(AffineBijection.ScaleHetero(factors))*/