package parallel

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
    //iter.foldLeft(mzero[B])(_ |+| f(_))

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
}
