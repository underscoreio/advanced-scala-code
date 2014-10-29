import scala.language.higherKinds

import scalaz.{Id, Monoid, MonadPlus, \/}
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

object FoldMap {
  def foldMapM[A, M[_] : MonadPlus, B: Monoid](iter: Iterable[A])(f: A => M[B] = (a: A) => a.point[Id]): M[B] =
    iter.foldLeft(mzero[B].point[M]){ (accum, elt) =>
      for {
        a <- accum
        b <- f(elt) <+> mzero[B].point[M]
      } yield a |+| b
    }

  implicit class IterableFoldMappable[A](iter: Iterable[A]) {
    def foldMapM[M[_] : MonadPlus, B : Monoid](f: A => M[B] = (a: A) => a.point[Id]): M[B] =
      FoldMap.foldMapM(iter)(f)
  }
}

object Examples {
  import FoldMap._
  import scala.concurrent.ExecutionContext.Implicits.global

  def examples = {
    import scalaz.std.anyVal._
    import scalaz.std.option._

    Seq(1, 2, 3).foldMapM(a => if(a % 2 == 0) some(a) else none[Int])
  }
}
