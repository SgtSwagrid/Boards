package boards.util

import boards.imports.circe.{*, given}
import boards.imports.math.{*, given}

object Codecs:
  
  private case class Entry[X](position: VecI, label: X)
  
  given [X: Encoder]: Encoder[Kernel[X]] =
    summon[Encoder[Seq[Entry[X]]]].contramap[Kernel[X]]: k =>
      k.positions.toSeq.map(v => Entry(v, k.label(v).get))
  
  given [X: Decoder]: Decoder[Kernel[X]] =
    summon[Decoder[Seq[Entry[X]]]].map[Kernel[X]]: entries =>
      Kernel[X](entries.map(e => (e.position, e.label)) *)
  
  given [X: Encoder]: Encoder[Vec[X]] = Encoder.encodeSeq[X].contramap(_.toSeq)
  given [X: Decoder: Ring]: Decoder[Vec[X]] = Decoder.decodeSeq[X].map(Vec.apply)