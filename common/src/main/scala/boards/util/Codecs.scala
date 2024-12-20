package boards.util

import boards.dsl.pieces.PieceRef.PieceId
import boards.imports.circe.{*, given}
import boards.imports.math.{*, given}
import boards.imports.games.{*, given}
import boards.math.region.RegionMap.RegionMapI
import boards.math.region.{Region, RegionMap, Vec}

object Codecs:
  
  given [X: Encoder]: Encoder[Vec[X]] =
    Encoder.encodeSeq[X].contramap(_.components)
  given [X: Decoder: Ring: Ordering]: Decoder[Vec[X]] =
    Decoder.decodeSeq[X].map(Vec.apply)
  
  given [X: Encoder]: Encoder[Region[X]] =
    summon[Encoder[List[Vec[X]]]].contramap[Region[X]]: region =>
      region.positions.toList
  
  given [X: Decoder: Ring: Ordering]: Decoder[Region[X]] =
    summon[Decoder[List[Vec[X]]]].map[Region[X]]: entries =>
      Region.from(entries)
      
  given [X: Encoder, A: Encoder]: Encoder[RegionMap[X, A]] =
    summon[Encoder[List[(Vec[X], A)]]].contramap[RegionMap[X, A]]: region =>
      region.entries.toList
  
  given [X: Decoder: Ring: Ordering, A: Decoder]: Decoder[RegionMap[X, A]] =
    summon[Decoder[List[(Vec[X], A)]]].map[RegionMap[X, A]]: entries =>
      RegionMap.from(entries)
  
  given Encoder[PlayerId] = summon[Encoder[Int]].contramap(_.toInt)
  given Decoder[PlayerId] = summon[Decoder[Int]].map(PlayerId.apply)
  
  given Encoder[TurnId] = summon[Encoder[Int]].contramap(_.toInt)
  given Decoder[TurnId] = summon[Decoder[Int]].map(TurnId.apply)
  
  given Encoder[PieceId] = summon[Encoder[Int]].contramap(_.toInt)
  given Decoder[PieceId] = summon[Decoder[Int]].map(PieceId.apply)