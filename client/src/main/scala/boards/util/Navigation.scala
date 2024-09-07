package boards.util

import org.scalajs.dom

object Navigation:
  
  def queryParams(url: String = dom.window.location.href): Map[String, String] =
    println(url)
    url.split("\\?").toSeq match
      case _ +: params +: _ =>
        params.split("&")
          .map:
            case s"$key=$value" => key -> value
          .toMap
      case _ => Map.empty
    
  def goto(url: String, options: (String, String)*): Unit =
    dom.window.location.href = url.split("\\?").head +
      (if options.isEmpty then "" else "?") +
      options.distinctBy((k, _) => k).map((k, v) => s"$k=$v").mkString("&")
    
  def gotoNext(): Unit =
    goto(queryParams().getOrElse("next", "/"))
    
  def reload(): Unit = goto("/")