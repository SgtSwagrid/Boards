package boards.util

import boards.imports.circe.{*, given}
import boards.imports.math.{*, given}
import boards.imports.games.{*, given}
import boards.math.Region

object Codecs:
  
  private case class Entry[X](position: VecI, label: X)
  
  given [X: Encoder]: Encoder[Region[Int, X]] =
    summon[Encoder[Seq[Entry[X]]]].contramap[Region[Int, X]]: k =>
      k.positions.toSeq.map(v => Entry(v, k.label(v).get))
  
  given [X: Decoder]: Decoder[Region[Int, X]] =
    summon[Decoder[Seq[Entry[X]]]].map[Region[Int, X]]: entries =>
      Region[Int, X](entries.map(e => (e.position, e.label))*)
  
  given [X : Encoder]: Encoder[Vec[X]] = Encoder.encodeSeq[X].contramap(_.components)
  given [X : Decoder : Ring : Ordering]: Decoder[Vec[X]] = Decoder.decodeSeq[X].map(Vec.apply)
  
  given Encoder[PlayerId] = summon[Encoder[Int]].contramap(_.toInt)
  given Decoder[PlayerId] = summon[Decoder[Int]].map(PlayerId.apply)