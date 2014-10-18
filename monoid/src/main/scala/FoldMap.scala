import scalaz.Monoid
import scalaz.syntax.monoid._

object FoldMap {
  implicit class ListFoldable[A : Monoid](base: List[A]) {
    def foldMap: A =
      base.foldLeft(mzero[A])(_ |+| _)
  }
}
