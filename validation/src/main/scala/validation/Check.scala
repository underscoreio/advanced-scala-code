package validation

object check {
  import cats.Semigroup
  import cats.data.Validated

  import predicate._

  sealed trait Check[E,A,B] {
    import Check._

    def map[C](f: B => C): Check[E,A,C] =
      Map[E,A,B,C](this, f)

    def flatMap[C](f: B => Check[E,A,C]) =
      FlatMap[E,A,B,C](this, f)

    def andThen[C](next: Check[E,B,C]): Check[E,A,C] =
      AndThen[E,A,B,C](this, next)

    def apply(in: A)(implicit s: Semigroup[E]): Validated[E,B]
  }
  object Check {
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
    final case class Pure[E,A,B](f: A => Validated[E,B]) extends Check[E,A,B] {
      def apply(in: A)(implicit s: Semigroup[E]): Validated[E,B] =
        f(in)
    }
    final case class PurePredicate[E,A,B](predicate: Predicate[E,A]) extends Check[E,A,A] {
      def apply(in: A)(implicit s: Semigroup[E]): Validated[E,A] =
        predicate(in)
    }

    def apply[E,A](predicate: Predicate[E,A]): Check[E,A,A] =
      PurePredicate(predicate)

    def apply[E,A,B](f: A => Validated[E,B]): Check[E,A,B] =
      Pure(f)
  }
}
