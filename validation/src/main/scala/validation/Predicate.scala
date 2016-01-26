package validation

object predicate {
  import cats.{Monoidal,Semigroup}
  import cats.data.{Validated,Xor}
  import cats.syntax.semigroup._ // For |+|
  import cats.syntax.monoidal._ // For |@|

  sealed trait Predicate[E,A] {
    import Predicate._

    def and(that: Predicate[E,A]): Predicate[E,A] =
      And(this, that)

    def or(that: Predicate[E,A]): Predicate[E,A] =
      Or(this, that)

    def zip[B](that: Predicate[E,B]): Predicate[E,(A,B)] =
      Zip(this, that)

    def run(implicit s: Semigroup[E]): A => Xor[E,A] =
      (a: A) => this.apply(a).toXor

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E,A]
  }
  object Predicate {
    final case class And[E,A](left: Predicate[E,A], right: Predicate[E,A]) extends Predicate[E,A] {
      def apply(a: A)(implicit s: Semigroup[E]): Validated[E,A] =
        (left(a) |@| right(a)) map { (_, _) => a }
    }
    final case class Or[E,A](left: Predicate[E,A], right: Predicate[E,A]) extends Predicate[E,A] {
      def apply(a: A)(implicit s: Semigroup[E]): Validated[E,A] =
      {
        import Validated._ // For Valid and Invalid
        left(a) match {
          case Valid(a1)   => Valid(a)
          case Invalid(e1) =>
            right(a) match {
              case Valid(a2)   => Valid(a)
              case Invalid(e2) => Invalid(e1 |+| e2)
            }
        }
      }
    }
    final case class Zip[E,A,B](left: Predicate[E,A], right: Predicate[E,B]) extends Predicate[E,(A,B)] {
      def apply(a: (A,B))(implicit s: Semigroup[E]): Validated[E,(A,B)] = {
        val (theA, theB) = a
        (left(theA) |@| right(theB)).tupled
      }
    }
    final case class Pure[E,A,B](f: A => Validated[E,B]) extends Predicate[E,A] {
      def apply(a: A)(implicit s: Semigroup[E]): Validated[E,A] =
        f(a) map (_ => a)
    }

    def apply[E,A,B](f: A => Validated[E,B]): Predicate[E,A] =
      Pure(f)

    def lift[E,A](msg: E)(pred: A => Boolean): Predicate[E,A] =
      Pure { (a: A) =>
        if(pred(a))
          Validated.valid(a)
        else
          Validated.invalid(msg)
      }
  }
}
