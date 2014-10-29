import scala.language.higherKinds

import scalaz.{Id, Monoid, Monad, MonadPlus, \/}
import scalaz.Id._
import scalaz.std.option._
import scalaz.syntax.monoid._
import scalaz.syntax.either._
import scalaz.syntax.monadPlus._

object MonadPlusExamples {
  some(3) <+> some(1)
  some(3) <+> none[Int]
  none[Int] <+> some(3)
}

object RecoveringFoldMap {
  def foldMapM[A, M[_] : MonadPlus, B: Monoid](iter: Iterable[A])(f: A => M[B] = (a: A) => a.point[Id]): M[B] =
    iter.foldLeft(mzero[B].point[M]){ (accum, elt) =>
      for {
        a <- accum
        b <- f(elt) <+> mzero[B].point[M]
      } yield a |+| b
    }

  implicit class IterableFoldMappable[A](iter: Iterable[A]) {
    def foldMapM[M[_] : MonadPlus, B : Monoid](f: A => M[B] = (a: A) => a.point[Id]): M[B] =
      RecoveringFoldMap.foldMapM(iter)(f)
  }
}

object RecoveringExamples {
  import RecoveringFoldMap._
  import scala.concurrent.ExecutionContext.Implicits.global

  def examples = {
    import scalaz.std.anyVal._
    import scalaz.std.option._

    Seq(1, 2, 3).foldMapM(a => if(a % 2 == 0) some(a) else none[Int])
  }
}

object RecoveringToolkit {
  def foldMapM[A, M[_] : Monad, B: Monoid](iter: Iterable[A])(f: A => M[B] = (a: A) => a.point[Id]): M[B] =
    iter.foldLeft(mzero[B].point[M]){ (accum, elt) =>
      for {
        a <- accum
        b <- f(elt)
      } yield a |+| b
    }

  implicit class IterableFoldMappable[A](iter: Iterable[A]) {
    def foldMapM[M[_] : Monad, B : Monoid](f: A => M[B] = (a: A) => a.point[Id]): M[B] =
      RecoveringToolkit.foldMapM(iter)(f)
  }

  def recover[A, M[_] : MonadPlus, B : Monoid](f: A => M[B]): (A => M[B]) = {
    val identity = mzero[B].point[M]
    a => (f(a) <+> identity)
  }

  def example = {
    import scalaz.std.anyVal._
    import scalaz.std.option._
    import scalaz.syntax.std.string._

    Seq("1", "b", "3").foldMapM(recover(_.parseInt.toOption))
  }
}
