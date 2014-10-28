import scalaz.{\/, OptionT, Monad}
import scalaz.syntax.monad._
import scalaz.std.option._

object OptionalResult {
  type Error[A] = \/[String, A]
  type Result[A] = OptionT[Error, A]

  val result: Result[Int] = 42.point[Result]
  val transformed =
    for {
      value <- result
    } yield value.toString

  val failed: Result[Int] = OptionT(none[Int].point[Error])
}
