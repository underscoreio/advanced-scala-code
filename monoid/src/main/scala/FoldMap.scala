import scalaz.Monoid
import scalaz.syntax.monoid._

object FoldMap {
  implicit class ListFoldable[A](base: List[A]) {
    def foldMap[B : Monoid](f: A => B = (a: A) => a): B =
      base.foldLeft(mzero[B])(_ |+| f(_))
  }
}
