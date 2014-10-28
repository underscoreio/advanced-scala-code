import scalaz.Applicative
import scalaz.std.option._
import scalaz.syntax.applicative._

object ApplicativeExample {
  some(1) <*> some((x: Int) => x + 1)

  (some(1) |@| some(2)){ _ + _ }
}
