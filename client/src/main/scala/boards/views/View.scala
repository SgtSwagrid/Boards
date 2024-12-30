package boards.views

import org.scalajs.dom.{HttpMethod, RequestInit, fetch}
import boards.imports.laminar.{*, given}
import boards.imports.circe.{*, given}
import boards.protocol.UserProtocol.User

import scala.concurrent.Await
import scala.concurrent.duration.Duration

abstract class View:
  
  def content(user: Option[User]): ReactiveElement.Base
  //def footer: ReactiveElement.Base = Footer()
  
  @JSExport("show")
  def show() =
    
    val root = document.getElementById("root")
    
    fetch("/auth/current", new RequestInit { method = HttpMethod.POST })
      .toFuture.flatMap(_.text().toFuture)
      .map(user => decode[Option[User]](user).toOption.flatten)
      .map(user => render(root, content(user)))
    