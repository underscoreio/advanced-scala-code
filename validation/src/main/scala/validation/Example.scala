package validation 

object example {
  import cats.data.{Kleisli,NonEmptyList,OneAnd,Validated,Xor}
  import cats.std.list._
  import cats.std.function._
  import cats.syntax.cartesian._
  import cats.syntax.validated._
  import predicate._

  type Error = NonEmptyList[String]
  def error(s: String): NonEmptyList[String] =
    OneAnd(s, Nil)

  type Result[A] = Xor[Error,A]
  type Check[A,B] = Kleisli[Result,A,B]
  // This constructor helps with type inference, which fails miserably in many cases below
  def Check[A,B](f: A => Result[B]): Check[A,B] =
    Kleisli(f)

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
  val checkUsername: Check[String,String] =
    Check((longerThan(3) and alphanumeric).run)

  // An email address must contain a single `@` sign. Split the string at the
  // `@`. The string to the left must not be empty. The string to the right must
  // be at least three characters long and contain a dot.
  val checkEmailAddress: Check[String,String] =
    Check { (string: String) =>
      string split '@' match {
        case Array(name, domain) => (name, domain).validNel[String].toXor
        case other => "Must contain a single @ character".invalidNel[(String,String)].toXor
      }
    } andThen Check[(String,String),(String,String)] {
        (longerThan(0) zip (longerThan(3) and contains('.'))).run
    } map {
      case (name, domain) => s"${name}@${domain}"
    }

  final case class User(name: String, email: String)
  def makeUser(name: String, email: String): Xor[Error,User] =
    (checkUsername.run(name) |@| checkEmailAddress.run(email)) map (User.apply _)

  def go() = {
    println(makeUser("Noel", "noel@underscore.io"))
    println(makeUser("", "noel@underscore@io"))
  }
}
