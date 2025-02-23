package boards.util

import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.codecs.StringAsIsCodec

object Tags:
  
  val dataTip: HtmlAttr[String] = htmlAttr("data-tip", StringAsIsCodec)
  
  val min: HtmlProp[String, String] = htmlProp("min", StringAsIsCodec)
  val max: HtmlProp[String, String] = htmlProp("max", StringAsIsCodec)
  val step: HtmlProp[String, String] = htmlProp("step", StringAsIsCodec)
