package parallel

import scala.language.higherKinds
import scala.collection.Iterable
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration

import scalaz.{Monad, Monoid}
import scalaz.Id._
import scalaz.syntax.monoid._
import scalaz.syntax.monad._

object FoldMap {
  def foldMap[A, B : Monoid](iter: Iterable[A])(f: A => B = (a: A) => a): B =
    foldMapM[A, Id, B](iter){ a => f(a).point[Id] }

  def foldMapM[A, M[_] : Monad, B: Monoid](iter: Iterable[A])(f: A => M[B] = (a: A) => a.point[Id]): M[B] =
    iter.foldLeft(mzero[B].point[M]){ (accum, elt) =>
      for {
        a <- accum
        b <- f(elt)
      } yield a |+| b
    }

  implicit class IterableFoldMappable[A](iter: Iterable[A]) {
    def foldMap[B : Monoid](f: A => B): B =
      FoldMap.foldMap(iter)(f)

    def foldMapM[M[_] : Monad, B : Monoid](f: A => M[B] = (a: A) => a.point[Id]): M[B] =
      FoldMap.foldMapM(iter)(f)
  }
}

object Timing {
  import FoldMap._
  import scala.concurrent.ExecutionContext.Implicits.global

  // Can we actually do work in parallel faster than in serial? Let's find out.
  def time[A](msg: String)(f: => A): A = {
    // Let Hotspot do some work
    f

    val start = System.nanoTime()
    val result = f
    val end = System.nanoTime()
    println(s"$msg took ${end - start} nanoseconds")
    result
  }

  def examples = {
    import scalaz.std.anyVal._
    import scalaz.std.option._
    import scalaz.std.list._

    val seq = Seq(1, 2, 3)
    seq.foldMapM(a => some(a))
    seq.foldMapM(a => List(a))
    seq.foldMap(a => if(a % 2 == 0) some(a) else none[Int])
  }
}
