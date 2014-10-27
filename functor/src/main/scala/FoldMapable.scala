import scala.language.higherKinds

import scalaz.Monoid
import scalaz.syntax.monoid._

trait FoldMapable[F[_]] {
  def foldMap[A, B : Monoid](fa: F[A])(f: A => B): B
}

object FoldMapable {
  def apply[F[_] : FoldMapable]: FoldMapable[F] =
    implicitly[FoldMapable[F]]

  implicit object ListFoldMapable extends FoldMapable[List] {
    def foldMap[A, B : Monoid](fa: List[A])(f: A => B): B =
      fa.foldLeft(mzero[B]){ _ |+| f(_) }
  }
}

object FoldMapableSyntax {
  implicit class IsFoldMapable[F[_] : FoldMapable, A](fa: F[A]) {
    def foldMap[B : Monoid](f: A => B): B =
      FoldMapable[F].foldMap(fa)(f)
  }
}
