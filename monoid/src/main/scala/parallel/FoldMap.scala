package parallel

import scala.collection.Iterable
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration

import scalaz.Monoid
import scalaz.syntax.monoid._

object FoldMap {
  def foldMapP[A, B : Monoid](iter: Iterable[A])(f: A => B = (a: A) => a)(implicit ec: ExecutionContext): B = {
    val nCores: Int = Runtime.getRuntime().availableProcessors()
    val groupSize: Int = (iter.size.toDouble / nCores.toDouble).ceil.round.toInt

    val groups = iter.grouped(groupSize)
    val futures: Iterator[Future[B]] = groups map { group =>
      Future { group.foldLeft(mzero[B])(_ |+| f(_)) }
    }
    val result: Future[B] = Future.sequence(futures) map { iterable =>
      iterable.foldLeft(mzero[B])(_ |+| _)
    }

    Await.result(result, Duration.Inf)
  }

  // Specialised implementation for arrays that doesn't copy
  def foldMapP[A, B : Monoid](arr: Array[A])(f: A => B)(implicit ec: ExecutionContext): B = {
    def iter(idx: Int, end: Int, result: B): B =
      if(idx == end)
        result
      else
        iter(idx + 1, end, result |+| f(arr(idx)))

    val nCores: Int = Runtime.getRuntime().availableProcessors()
    val groupSize: Int = (arr.size.toDouble / nCores.toDouble).ceil.round.toInt

    val futures =
      for(i <- 0 until nCores) yield {
        Future {
          iter(i * groupSize, i * groupSize + groupSize, mzero[B])
        }
      }
    val result: Future[B] = Future.sequence(futures) map { iterable =>
      iterable.foldLeft(mzero[B])(_ |+| _)
    }

    Await.result(result, Duration.Inf)
  }

  def foldMap[A, B : Monoid](iter: Iterable[A])(f: A => B = (a: A) => a): B =
    iter.foldLeft(mzero[B])(_ |+| f(_))

  implicit class IterableFoldMappable[A](iter: Iterable[A]) {
    def foldMapP[B : Monoid](f: A => B = (a: A) => a)(implicit ec: ExecutionContext): B =
      FoldMap.foldMap(iter)(f)

    def foldMap[B : Monoid](f: A => B = (a: A) => a): B =
      FoldMap.foldMap(iter)(f)
  }

  implicit class ArrayFoldMappable[A](arr: Array[A]) {
    def foldMapP[B : Monoid](f: A => B = (a: A) => a)(implicit ec: ExecutionContext): B =
      FoldMap.foldMap(arr)(f)

    def foldMap[B : Monoid](f: A => B = (a: A) => a): B =
      FoldMap.foldMap(arr)(f)
  }
}

object Timing {
  import FoldMap._
  //import scala.concurrent.ExecutionContext.Implicits.global
  import java.util.concurrent.Executors
  val threadPool = Executors.newFixedThreadPool(4)
  implicit val ec = ExecutionContext.fromExecutor(threadPool)

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

  def counting(n: Int = 10000000) = {
    import scalaz.std.anyVal._

    val data = Vector.fill(n)(1)

    time("Counting items in parallel") { data.foldMapP[Int]() }
    time("Counting items in serial  ") { data.foldMap[Int]() }
  }

  def complicatedCounting(n: Int = 10000000) = {
    import scalaz.std.anyVal._

    val data = Vector.fill(n)("1")

    time("Counting items in parallel") { data.foldMapP[Int](_.toInt) }
    time("Counting items in serial  ") { data.foldMap[Int](_.toInt) }
  }

  def countingArray(n: Int = 10000000) = {
    import scalaz.std.anyVal._

    val data = Array.fill(n)(1)

    time("Counting items in parallel") { data.foldMapP[Int]() }
    time("Counting items in serial  ") { data.foldMap[Int]() }
  }

  def complicatedCountingArray(n: Int = 10000000) = {
    import scalaz.std.anyVal._

    val data = Array.fill(n)("1")

    time("Counting items in parallel") { data.foldMapP[Int](_.toInt) }
    time("Counting items in serial  ") { data.foldMap[Int](_.toInt) }
  }
}
