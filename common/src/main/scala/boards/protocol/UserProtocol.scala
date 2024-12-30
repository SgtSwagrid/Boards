package boards.protocol

object UserProtocol:
  
  private val EMAIL_REGEX = """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r
  
  case class User (
    userId: Int,
    username: String,
    joined: Long,
  )
  
  case class LoginForm (
    username: String,
    password: String
  )
  
  enum LoginError:
    case InvalidUsername, InvalidPassword
    
  type LoginResponse = Either[LoginError, Int]
  
  case class RegistrationForm (
    username: String,
    email: String,
    password: String,
    passwordRepeat: String,
  ):
    import RegistrationError.*
    import Limits.*
    
    def validate: Either[RegistrationError, Unit] = for
      _ <- validateUsername
      _ <- validateEmail
      _ <- validatePassword
    yield ()
    
    def validateUsername: Either[RegistrationError, Unit] = for
      _ <- Either.cond(username.length >= MIN_LENGTH_USERNAME, (),
        UsernameTooShort(username, username.length, MIN_LENGTH_USERNAME))
      _ <- Either.cond(username.length <= MAX_LENGTH_USERNAME, (),
        UsernameTooLong(username, username.length, MAX_LENGTH_USERNAME))
    yield ()
      
    def validateEmail: Either[RegistrationError, Unit] = for
      _ <- Either.cond(email.length <= Limits.MAX_LENGTH_EMAIL, (),
        EmailTooLong(email, email.length, Limits.MAX_LENGTH_EMAIL))
      _ <- Either.cond(EMAIL_REGEX.matches(email), (),
        EmailInvalid(email))
    yield ()
    
    def validatePassword: Either[RegistrationError, Unit] = for
      _ <- Either.cond(password.length >= MIN_LENGTH_PASSWORD, (),
        PasswordTooShort(password.length, MIN_LENGTH_PASSWORD))
      _ <- Either.cond(password.length <= MAX_LENGTH_PASSWORD, (),
        PasswordTooLong(password.length, MAX_LENGTH_PASSWORD))
      _ <- Either.cond(password == passwordRepeat, (), PasswordMismatch)
    yield ()
  
  enum RegistrationError:
    case UsernameTaken(username: String)
    case UsernameTooShort(username: String, length: Int, minLength: Int)
    case UsernameTooLong(username: String, length: Int, maxLength: Int)
    case EmailTaken(email: String)
    case EmailInvalid(email: String)
    case EmailTooLong(email: String, length: Int, maxLength: Int)
    case PasswordTooShort(length: Int, minLength: Int)
    case PasswordTooLong(length: Int, maxLength: Int)
    case PasswordMismatch
    
  type RegistrationResponse = Either[RegistrationError, Int]
    
  object Limits:
    val MIN_LENGTH_USERNAME = 3
    val MAX_LENGTH_USERNAME = 20
    val MAX_LENGTH_EMAIL = 60
    val MIN_LENGTH_PASSWORD = 6
    val MAX_LENGTH_PASSWORD = 30