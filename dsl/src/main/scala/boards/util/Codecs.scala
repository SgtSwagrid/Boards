package boards.util

import boards.math.algebra.Algebra.{*, given}
import boards.math.vector.{Bounds, Region, RegionMap, Vec}
import boards.dsl.pieces.PieceRef.PieceId
import boards.dsl.meta.PlayerRef.PlayerId
import boards.dsl.meta.TurnId
import boards.dsl.meta.TurnId.TurnId
import io.circe.*
import io.circe.syntax.*
import io.circe.generic.auto.*
import io.circe.parser.*
import io.circe.Encoder.{encodeEither, encodeOption, encodeSeq, encodeSet}
import io.circe.Decoder.{decodeEither, decodeOption, decodeSeq, decodeSet}

object Codecs:
  
  given [X: Encoder]: Encoder[Region[X]] =
    summon[Encoder[List[Vec[X]]]].contramap[Region[X]]: region =>
      region.positions.toList
  
  given [X: {Decoder, Numeric}]: Decoder[Region[X]] =
    summon[Decoder[List[Vec[X]]]].map[Region[X]]: entries =>
      Region.from(entries)
      
  given [X: Encoder, A: Encoder]: Encoder[Map[X, A]] =
    summon[Encoder[List[(X, A)]]].contramap[Map[X, A]](_.toList)
    
  given [X: Decoder, A: Decoder]: Decoder[Map[X, A]] =
    summon[Decoder[List[(X, A)]]].map(_.toMap)
      
  /*given [X: Encoder, A: Encoder]: Encoder[RegionMap[X, A]] =
    summon[Encoder[List[(Vec[X], A)]]].contramap[RegionMap[X, A]]: region =>
      region.entries.toList
  
  given [X: {Decoder, Numeric}, A: Decoder]: Decoder[RegionMap[X, A]] =
    summon[Decoder[List[(Vec[X], A)]]].map[RegionMap[X, A]]: entries =>
      RegionMap.from(entries)*/
  
  /*given [X: Encoder]: Encoder[BoundingBox[X]] =
    summon[Encoder[Option[(Vec[X], Vec[X])]]].contramap:
      case BoundingBox.NonEmpty(start, end) => Some((start, end))
      case BoundingBox.Empty() => None
  
  given [X: {Decoder, Numeric}]: Decoder[BoundingBox[X]] =
    summon[Decoder[Option[(Vec[X], Vec[X])]]].map:
      case Some((start, end)) => BoundingBox.NonEmpty(start, end)
      case None => BoundingBox.Empty()*/
      
  /*given Encoder[Rational] =
    summon[Encoder[(Int, Int)]].contramap:
      case Rational(n, d) => (n, d)
      
  given Decoder[Rational] =
    summon[Decoder[(Int, Int)]].map:
      case (n, d) => Rational.fraction(n, d)
      
  given Encoder[Surd] =
    summon[Encoder[Seq[Surd.SurdTerm]]].contramap:
      case Surd(terms) => terms
      
  given Decoder[Surd] =
    summon[Decoder[Seq[Surd.SurdTerm]]].map(Surd.apply)*/
  
      
  /*val z = summon[Encoder[Surd]]
  val y = summon[Encoder[Colour]]
  val a = summon[Encoder[EmbeddedRegion.Tile]]
  val b = summon[Encoder[EmbeddedRegion.ExplicitEmbedding]]
  val c = summon[Encoder[EmbeddedRegion.RectilinearEmbedding]]
  //val d = summon[Encoder[EmbeddedRegion]]
  val f = summon[Encoder[EmbeddedRegion.PaintedEmbedding]]
  val e = summon[Encoder[Map[Int, Int]]]*/
  
  
  given Encoder[PlayerId] = summon[Encoder[Int]].contramap(_.toInt)
  given Decoder[PlayerId] = summon[Decoder[Int]].map(PlayerId.apply)
  
  given Encoder[TurnId] = summon[Encoder[Int]].contramap(_.toInt)
  given Decoder[TurnId] = summon[Decoder[Int]].map(TurnId.apply)
  
  given Encoder[PieceId] = summon[Encoder[Int]].contramap(_.toInt)
  given Decoder[PieceId] = summon[Decoder[Int]].map(PieceId.apply)