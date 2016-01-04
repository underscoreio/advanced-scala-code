package validation

object check {
  import cats.Semigroup
  import cats.data.Validated

  import predicate._

  sealed trait Check[E,A,B] {
    def map[C](f: B => C): Check[E,A,C] =
      Map[E,A,B,C](this, f)

    def flatMap[C](f: B => Check[E,A,C]) =
      FlatMap[E,A,B,C](this, f)

    def apply(in: A)(implicit s: Semigroup[E]): Validated[E,B]
  }
  final case class Map[E,A,B,C](check: Check[E,A,B], f: B => C) extends Check[E,A,C] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E,C] =
      check(in) map f
  }
  final case class Pure[E,A](predicate: Predicate[E,A]) extends Check[E,A,A] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E,A] =
      predicate(in)
  }
  final case class FlatMap[E,A,B,C](check: Check[E,A,B], f: B => Check[E,A,C]) extends Check[E,A,C] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E,C] =
      check(in).withXor { _.flatMap (a => f(a)(in).toXor) }
  }
}
