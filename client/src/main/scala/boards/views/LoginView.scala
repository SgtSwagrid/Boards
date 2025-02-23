package boards.views

import boards.components.{Footer, InputField, Navbar, SVG, Tabs}
import boards.components.Tabs.Tab
import com.raquo.laminar.api.L.*
import boards.protocol.UserProtocol.{LoginError, LoginForm, LoginResponse, RegistrationError, RegistrationForm, RegistrationResponse, User}
import boards.protocol.UserProtocol.LoginError.*
import boards.protocol.UserProtocol.RegistrationError.*
import boards.util.Navigation
import io.laminext.fetch.circe.Fetch
import org.scalajs.dom.KeyCode
import io.laminext.fetch.circe.jsonRequestBody
import io.circe.parser.{decode, parse}
import io.laminext.fetch.circe.fetchEventStreamBuilderSyntaxCirce
import scala.concurrent.ExecutionContext.Implicits.global

import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("LoginView")
object LoginView extends View:
  
  def content (user: Option[User]) =
    val loginError = Var[Option[LoginError]](None)
    val registerError = Var[Option[RegistrationError]](None)
    div (
      Navbar(user),
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
      
      child.maybe <-- error.map: e =>
        e.map:
          case InvalidUsername => "No such user exists."
          case InvalidPassword => "Password for user is incorrect."
      .map(_.map(error => p(className("text-error"), error))),
      
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
      val form = RegistrationForm(username.now(), email.now(), password1.now(), password2.now())
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
      
      child.maybe <-- error.map: e =>
        e.map:
          case UsernameTaken(name) => s"The username '$name' is already taken."
          case UsernameTooShort(_, _, min) => s"The username may not be less than $min characters."
          case UsernameTooLong(_, _, max) => s"The username may not be more than $max characters."
          case EmailTaken(email) => s"The email address '$email' is already in use."
          case EmailInvalid(email) => s"'$email' is not a valid email address."
          case EmailTooLong(_, _, max) => s"The email address may not be more than $max characters."
          case PasswordTooShort(_, min) => s"The password may not be less than $min characters."
          case PasswordTooLong(_, max) => s"The password may not be more than $max characters."
          case PasswordMismatch => "The passwords do not match."
      .map(_.map(error => p(className("text-error"), error))),
      
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