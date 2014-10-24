import scalaz.\/
import scalaz.syntax.monad._
import scalaz.syntax.either._

object EitherExample {

  def flatMap =
    for {
      x <- 1.right[String]
      y <- (x + 2).right[String]
    } yield y * 3

  def basicMethods = {
    1.right[String].fold(
      l = l => "We failed :(",
      r = r => s"We succeeded with $r"
    )
    1.right[String].getOrElse(0)

    1.right[String] orElse 2.right[String]
    "Error".left[Int] orElse 2.right[String]
  }

}
