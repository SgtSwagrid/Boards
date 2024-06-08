package boards.views

import boards.components.{Tabs, *}
import boards.components.Tabs.Tab
import boards.protocol.UserProtocol.*
import boards.util.Navigation
import boards.util.Navigation.gotoNext
import com.raquo.airstream.web.FetchOptions
import org.scalajs.dom

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.nodes.ReactiveElement
import com.raquo.laminar.api.features.unitArrows
import com.raquo.laminar.keys.StyleVendorPrefixes.o
import io.laminext.syntax.core.*
import io.laminext.fetch.circe.*
import io.circe.*
import io.circe.syntax.*
import io.circe.parser.*
import io.circe.generic.auto.*
import org.scalajs.dom.{KeyCode, KeyboardEvent}

import scala.concurrent.ExecutionContext.Implicits.global

import LoginError.*
import RegistrationError.*

@JSExportTopLevel("LoginView")
object LoginView extends View:
  
  def content =
    val loginError = Var[Option[LoginError]](None)
    val registerError = Var[Option[RegistrationError]](None)
    div (
      Navbar(),
      div (paddingTop("100px"),
        div (className("card w-96 bg-neutral shadow-xl mx-auto"),
          div (className("card-body"),
            Tabs (
              Tab(p("Login"), loginForm),
              Tab(p("Register"), registerForm)
            )
          )
        )
      ),
      Footer()
    )
  
  private def loginForm =
    
    val username = Var("")
    val password = Var("")
    
    val submit = new EventBus[Unit]
    
    val result = submit.events.flatMapTo:
      val form = LoginForm(username.now(), password.now())
      Fetch.post("/auth/login", body=form)
        .decode[LoginResponse].map(_.data)
      
    val loading = submit.events.mapTo(true)
      .mergeWith(result.delay(250).mapTo(false))
      .toSignal(false)
    
    val error = result.map(_.swap.toOption)
    
    div (
      
      img (
        className("mx-auto"),
        src("/assets/images/ui/auth/login.svg"),
        width("200px"),
        height("200px")
      ),
      
      InputField("Username", SVG.Username, username, focus=true),
      InputField("Password", SVG.Password, password, "password"),
      
      child.maybe <-- error.optionMap:
        case InvalidUsername => "No such user exists."
        case InvalidPassword => "Password for user is incorrect."
      .optionMap(error => p(className("text-error"), error)),
      
      button (
        className("btn btn-primary"),
        width("100%"),
        marginTop("10px"),
        child <-- loading.map:
          case false => "Login"
          case true => span(className("loading loading-dots loading-md"))
        ,
        onClick.mapToUnit --> submit
      ),
      onKeyDown.filter(_.keyCode == KeyCode.Enter).mapToUnit --> submit,
      result --> (r => if r.isRight then Navigation.gotoNext() else ())
    )
  
  private def registerForm =
    
    val username = Var("")
    val email = Var("")
    val password1 = Var("")
    val password2 = Var("")
    
    val submit = new EventBus[Unit]
    
    val result = submit.events.flatMapTo:
      val form = RegistrationForm(username.now(), email.now(), password1.now())
      Fetch.post("/auth/register", body=form)
        .decode[RegistrationResponse].map(_.data)
    
    val loading = submit.events.mapTo(true)
      .mergeWith(result.delay(250).mapTo(false))
      .toSignal(false)
    
    val error = result.map(_.swap.toOption)
    
    div (
      
      img (
        className("mx-auto"),
        src("/assets/images/ui/auth/register.svg"),
        width("150px"),
        height("150px"),
        marginBottom("30px")
      ),
      
      InputField("Username", SVG.Username, username, focus=true),
      InputField("Email", SVG.Email, email),
      InputField("Password", SVG.Password, password1, "password"),
      InputField("Repeat Password", SVG.Password, password2, "password"),
      
      child.maybe <-- error.optionMap:
        case UsernameTaken(name) => s"The username '$name' is already taken."
        case UsernameTooShort(_, _, min) => s"The username may not be less than $min characters."
        case UsernameTooLong(_, _, max) => s"The username may not be more than $max characters."
        case EmailTaken(email) => s"The email address '$email' is already in use."
        case EmailInvalid(email) => s"'$email' is not a valid email address."
        case EmailTooLong(_, _, max) => s"The email address may not be more than $max characters."
        case PasswordTooShort(_, min) => s"The password may not be less than $min characters."
        case PasswordTooLong(_, max) => s"The password may not be more than $max characters."
      .optionMap(error => p(className("text-error"), error)),
      
      button (
        className("btn btn-primary"),
        width("100%"),
        marginTop("10px"),
        child <-- loading.map:
          case false => "Register"
          case true => span(className("loading loading-dots loading-md"))
        ,
        onClick.mapToUnit --> submit
      ),
      onKeyDown.filter(_.keyCode == KeyCode.Enter).mapToUnit --> submit,
      result --> (r => if r.isRight then Navigation.gotoNext() else ())
    )