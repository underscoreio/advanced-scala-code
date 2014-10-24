import scalaz.Monad
import scalaz.std.option._
import scalaz.std.list._
import scalaz.syntax.monad._

object SyntaxExamples {
  val option = 3.point[Option]
  val f = ((x: Int) => x + 1).lift(Monad[Option])

  f(option)
  option >>= ((x: Int) => (x + 39).point[Option])
  option >> (42.point[Option])
}

object InterfaceExamples {
  val option = Monad[Option].point(3)
  val f = Monad[Option].lift((x: Int) => x + 1)
  val tupled: Option[(Int, String, Double)] =
    Monad[Option].tuple3(some(1), some("hi"), some(3.0))
  val sequence: Option[List[Int]] =
    Monad[Option].sequence(List(some(1), some(2), some(3)))

  f(option)
}
