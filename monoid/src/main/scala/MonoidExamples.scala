/* Examples of Monoid functionality in Scalaz */

import scalaz.Monoid

object MonoidExamples {

  def interface = {
    import scalaz.std.string._ // Get String instances into scope
    val instance = Monoid[String]
    instance.append("Hi there", instance.zero)
  }

  def syntax = {
    import scalaz.syntax.monoid._
    import scalaz.std.string._
    import scalaz.std.anyVal._

    "Hi " |+| "there" |+| mzero[String]
    1 |+| 2 |+| mzero[Int]
  }

}
