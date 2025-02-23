package boards.views

import org.scalajs.dom.{document, fetch, HttpMethod, RequestInit}
import boards.protocol.UserProtocol.User
import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.nodes.ReactiveElement

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.scalajs.js.annotation.JSExport
import io.circe.parser.{decode, parse}

import scala.concurrent.ExecutionContext.Implicits.global

abstract class View:
  
  def content(user: Option[User]): ReactiveElement.Base
  //def footer: ReactiveElement.Base = Footer()
  
  @JSExport("show")
  def show () =
    
    val root = document.getElementById("root")
    
    fetch("/auth/current", new RequestInit { method = HttpMethod.POST })
      .toFuture.flatMap(_.text().toFuture)
      .map(user => decode[Option[User]](user).toOption.flatten)
      .map(user => render(root, content(user)))
    