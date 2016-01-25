package validation 

object example {
  import cats.data.{NonEmptyList,OneAnd,Validated}
  import cats.std.list._
  import cats.syntax.monoidal._
  import cats.syntax.validated._
  import check._
  import predicate._

  type Error = NonEmptyList[String]
  def error(s: String): NonEmptyList[String] =
    OneAnd(s, Nil)

  // Utilities. We could implement all the checks using regular expressions but
  // this shows off the compositionality of the library.
  def longerThan(n: Int): Predicate[Error,String] =
    Predicate.lift(error(s"Must be longer than $n characters")){ _.size > n }

  val alphanumeric: Predicate[Error,String] =
    Predicate.lift(error(s"Must be all alphanumeric characters")){ _.forall(_.isLetterOrDigit) }

  def contains(char: Char): Predicate[Error,String] =
    Predicate.lift(error(s"Must contain the character $char")){ _.contains(char) }

  def containsOnce(char: Char): Predicate[Error,String] =
    Predicate.lift(error(s"Must contain the character $char only once")){
      _.filter(c => c == char).size == 1
    }

  // A username must contain at least four characters and consist entirely of
  // alphanumeric characters
  val checkUsername: Check[Error,String,String] =
    Check(longerThan(3) and alphanumeric)

  // An email address must contain a single `@` sign. Split the string at the
  // `@`. The string to the left must not be empty. The string to the right must
  // be at least three characters long and contain a dot.
  val checkEmailAddress: Check[Error,String,String] =
    Check { (string: String) =>
      string split '@' match {
        case Array(name, domain) => (name, domain).validNel[String]
        case other => "Must contain a single @ character".invalidNel[(String,String)]
      }
    } andThen Check[Error,(String,String),(String,String)] { case (name, domain) =>
        ((longerThan(0))(name) |@| (longerThan(3) and contains('.'))(domain)).tupled
    } map {
      case (name, domain) => s"${name}@${domain}"
    }

  final case class User(name: String, email: String)
  def makeUser(name: String, email: String): Validated[Error,User] =
    (checkUsername(name) |@| checkEmailAddress(email)) map (User.apply _)

  def go() = {
    println(makeUser("Noel", "noel@underscore.io"))
    println(makeUser("", "noel@underscore@io"))
  }

}
