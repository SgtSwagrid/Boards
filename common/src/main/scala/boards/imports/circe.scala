package boards.imports

object circe:
  
  export io.circe.{Encoder, Decoder, Codec, Json, JsonObject}
  export io.circe.syntax.{EncoderOps, KeyOps}
  export io.circe.generic.auto.*
  export io.circe.parser.{decode, parse}
  export io.circe.Encoder.{encodeOption, encodeSeq, encodeMap, encodeEither, encodeSet}
  export boards.util.Codecs.given