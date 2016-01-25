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

    def andThen[C](next: Check[E,B,C]): Check[E,A,C] =
      AndThen[E,A,B,C](this, next)

    def apply(in: A)(implicit s: Semigroup[E]): Validated[E,B]
  }
  object Check {
    def apply[E,A](pred: Predicate[E,A]): Check[E,A,A] =
      Predicate(pred)

    def apply[E,A,B](f: A => Validated[E,B]): Check[E,A,B] =
      Function(f)
  }

  final case class Map[E,A,B,C](check: Check[E,A,B], f: B => C) extends Check[E,A,C] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E,C] =
      check(in) map f
  }
  final case class FlatMap[E,A,B,C](check: Check[E,A,B], f: B => Check[E,A,C]) extends Check[E,A,C] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E,C] =
      check(in).withXor { _.flatMap (b => f(b)(in).toXor) }
  }
  final case class AndThen[E,A,B,C](check: Check[E,A,B], next: Check[E,B,C]) extends Check[E,A,C] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E,C] =
      check(in).withXor { _.flatMap (b => next(b).toXor) }
  }
  final case class Predicate[E,A](predicate: Predicate[E,A]) extends Check[E,A,A] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E,A] =
      predicate(in)
  }
  final case class Function[E,A,B](f: A => Validated[E,B]) extends Check[E,A,B] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E,B] =
      f(in)
  }
}
