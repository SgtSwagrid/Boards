package boards.math

import boards.math.Interval.{IntervalD, IntervalF, IntervalI, IntervalL, IntervalR, IntervalS, UInterval, UIntervalD, UIntervalF, UIntervalI, UIntervalL, UIntervalR, UIntervalS}
import boards.math.region.BoundingBox.{BoundingBoxD, BoundingBoxF, BoundingBoxI, BoundingBoxL, BoundingBoxR, BoundingBoxS, UBoundingBox, UBoundingBoxD, UBoundingBoxF, UBoundingBoxI, UBoundingBoxL, UBoundingBoxR, UBoundingBoxS}
import boards.math.region.Vec.{UVec, UVecD, UVecF, UVecI, UVecL, UVecR, UVecS, VecD, VecF, VecI, VecL, VecR, VecS}
import boards.math.region.{BoundingBox, Vec}
import boards.math.Unbounded.{UDouble, UFloat, UInt, ULong, URational, USurd}
import boards.math.Algebra.{*, given}

object Conversions:
    
  @FunctionalInterface
  trait ToInt [X]:
    def convertToLong (x: X): Long
    extension (x: X) def toInt: Int = convertToLong(x).toInt
    extension (x: X) def toLong: Long = convertToLong(x)
  
  @FunctionalInterface
  trait ToFloat [X]:
    def convertToDouble (x: X): Double
    extension (x: X) def toFloat: Float = convertToDouble(x).toFloat
    extension (x: X) def toDouble: Double = convertToDouble(x)
  
  @FunctionalInterface
  trait ToRational [X]:
    def convertToRational (x: X): Rational
    extension (x: X) def toRational: Rational = convertToRational(x)
  
  @FunctionalInterface
  trait ToSurd [X]:
    def convertToSurd (x: X): Surd
    extension (x: X) def toSurd: Surd = convertToSurd(x)
  
  given ToInt[Int] = identity
  given ToInt[Float] = _.toInt
  given ToInt[Rational] = _.previousInt
  given ToInt[Surd] = _.approximate.toInt
  
  given ToFloat[Int] = _.toFloat
  given ToFloat[Float] = identity
  given ToFloat[Rational] = _.approximate
  given ToFloat[Surd] = _.approximate
  
  given ToRational[Int] = Rational.integer
  given ToRational[Rational] = identity
  
  given ToSurd[Int] = Surd.integer
  given ToSurd[Rational] = Surd.rational
  given ToSurd[Surd] = identity
  
  extension [X: ToInt] (v: Vec[X])
    def toVecI: VecI = v.map(_.toInt)
    def toVecL: VecL = v.map(_.toLong)
    
  extension [X: ToFloat] (v: Vec[X])
    def toVecF: VecF = v.map(_.toFloat)
    def toVecD: VecD = v.map(_.toDouble)
    
  extension [X: ToRational] (v: Vec[X])
    def toVecR: VecR = v.map(_.toRational)
    
  extension [X: ToSurd] (v: Vec[X])
    def toVecS: VecS = v.map(_.toSurd)
    
  extension [X: ToInt] (i: Interval[X])
    def toIntervalI: IntervalI = i.map(_.toInt)
    def toIntervalL: IntervalL = i.map(_.toLong)
    
  extension [X: ToFloat] (i: Interval[X])
    def toIntervalF: IntervalF = i.map(_.toFloat)
    def toIntervalD: IntervalD = i.map(_.toDouble)
    
  extension [X: ToRational] (i: Interval[X])
    def toIntervalR: IntervalR = i.map(_.toRational)
    
  extension [X: ToSurd] (i: Interval[X])
    def toIntervalS: IntervalS = i.map(_.toSurd)
    
  extension [X: ToInt] (bb: BoundingBox[X])
    def toBoundingBoxI: BoundingBoxI = bb.map(_.toVecI)
    def toBoundingBoxL: BoundingBoxL = bb.map(_.toVecL)
    
  extension [X: ToFloat] (bb: BoundingBox[X])
    def toBoundingBoxF: BoundingBoxF = bb.map(_.toVecF)
    def toBoundingBoxD: BoundingBoxD = bb.map(_.toVecD)
    
  extension [X: ToRational] (bb: BoundingBox[X])
    def toBoundingBoxR: BoundingBoxR = bb.map(_.toVecR)
  
  extension [X: ToSurd] (bb: BoundingBox[X])
    def toBoundingBoxS: BoundingBoxS = bb.map(_.toVecS)
    
  extension [X: ToInt] (x: Unbounded[X])
    def toUInt: UInt = x.map(_.toInt)
    def toULong: ULong = x.map(_.toLong)
    
  extension [X: ToFloat] (x: Unbounded[X])
    def toUFloat: UFloat = x.map(_.toFloat)
    def toUDouble: UDouble = x.map(_.toDouble)
    
  extension [X: ToRational] (x: Unbounded[X])
    def toURational: URational = x.map(_.toRational)
    
  extension [X: ToSurd] (x: Unbounded[X])
    def toUSurd: USurd = x.map(_.toSurd)
    
  extension [X: ToInt] (v: UVec[X])
    def toUVecI: UVecI = v.map(_.toUInt)
    def toUVecL: UVecL = v.map(_.toULong)
    
  extension [X: ToFloat] (v: UVec[X])
    def toUVecF: UVecF = v.map(_.toUFloat)
    def toUVecD: UVecD = v.map(_.toUDouble)
    
  extension [X: ToRational] (v: UVec[X])
    def toUVecR: UVecR = v.map(_.toURational)
    
  extension [X: ToSurd] (v: UVec[X])
    def toUVecS: UVecS = v.map(_.toUSurd)
    
  extension [X: ToInt] (i: UInterval[X])
    def toUIntervalI: UIntervalI = i.map(_.map(_.toInt))
    def toUIntervalL: UIntervalL = i.map(_.map(_.toLong))
    
  extension [X: ToFloat] (i: UInterval[X])
    def toUIntervalF: UIntervalF = i.map(_.map(_.toFloat))
    def toUIntervalD: UIntervalD = i.map(_.map(_.toDouble))
    
  extension [X: ToRational] (i: UInterval[X])
    def toUIntervalR: UIntervalR = i.map(_.map(_.toRational))
    
  extension [X: ToSurd] (i: UInterval[X])
    def toUIntervalS: UIntervalS = i.map(_.map(_.toSurd))
    
  extension [X: ToInt] (bb: UBoundingBox[X])
    def toUBoundingBoxI: UBoundingBoxI = bb.map(_.toUVecI)
    def toUBoundingBoxL: UBoundingBoxL = bb.map(_.toUVecL)
    
  extension [X: ToFloat] (bb: UBoundingBox[X])
    def toUBoundingBoxF: UBoundingBoxF = bb.map(_.toUVecF)
    def toUBoundingBoxD: UBoundingBoxD = bb.map(_.toUVecD)
    
  extension [X: ToRational] (bb: UBoundingBox[X])
    def toUBoundingBoxR: UBoundingBoxR = bb.map(_.toUVecR)
    
  extension [X: ToSurd] (bb: UBoundingBox[X])
    def toUBoundingBoxS: UBoundingBoxS = bb.map(_.toUVecS)
    
  extension [X: OrderedRing] (x: X)
    def toUnbounded: Unbounded[X] = Unbounded.Finite(x)
    
  extension [X: OrderedRing] (v: Vec[X])
    def toUnbounded: UVec[X] = v.map(Unbounded.Finite.apply)
    
  extension [X: OrderedRing] (v: UVec[X])
    def toBounded: Vec[X] = v.map(_.toBounded)
    
  extension [X: OrderedRing] (i: Interval[X])
    def toUnbounded: UInterval[X] = i.map(_.toUnbounded)
    
  extension [X: OrderedRing] (i: UInterval[X])
    def toBounded: Interval[X] = i.map(_.toBounded)
    
  extension [X: OrderedRing] (bb: BoundingBox[X])
    def toUnbounded: UBoundingBox[X] = bb.map(_.toUnbounded)
    
  extension [X: OrderedRing] (bb: UBoundingBox[X])
    def toBounded: BoundingBox[X] = bb.map(_.toBounded)
    
  /*given Conversion[Int, Rational] = _.toRational
  given Conversion[Int, Surd] = _.toSurd
  given Conversion[Rational, Surd] = _.toSurd
  
  given Conversion[VecI, VecF] = _.toVecF
  given Conversion[VecI, VecL] = _.toVecL
  given Conversion[VecI, VecR] = _.toVecR
  given Conversion[VecI, VecS] = _.toVecS
  given Conversion[VecR, VecS] = _.toVecS
  
  given Conversion[IntervalI, IntervalF] = _.toIntervalF
  given Conversion[IntervalI, IntervalL] = _.toIntervalL
  given Conversion[IntervalI, IntervalR] = _.toIntervalR
  given Conversion[IntervalI, IntervalS] = _.toIntervalS
  given Conversion[IntervalR, IntervalS] = _.toIntervalS
  
  given Conversion[BoundingBoxI, BoundingBoxF] = _.toBoundingBoxF
  given Conversion[BoundingBoxI, BoundingBoxL] = _.toBoundingBoxL
  given Conversion[BoundingBoxI, BoundingBoxR] = _.toBoundingBoxR
  given Conversion[BoundingBoxI, BoundingBoxS] = _.toBoundingBoxS
  given Conversion[BoundingBoxR, BoundingBoxS] = _.toBoundingBoxS
  
  given [X: OrderedRing]: Conversion[X, Unbounded[X]] = _.toUnbounded
  given [X: OrderedRing]: Conversion[Vec[X], UVec[X]] = _.toUnbounded
  given [X: OrderedRing]: Conversion[Interval[X], UInterval[X]] = _.toUnbounded
  given [X: OrderedRing]: Conversion[BoundingBox[X], UBoundingBox[X]] = _.toUnbounded*/