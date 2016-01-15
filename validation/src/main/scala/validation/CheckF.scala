package validation

import scalaz.{\/,-\/,\/-}
import scalaz.Semigroup
import scalaz.syntax.either._ // For .left and .right
import scalaz.syntax.semigroup._ // For |+|

final case class CheckF[E,A](f: A => E \/ A) {
  def and(that: CheckF[E,A])(implicit s: Semigroup[E]): CheckF[E,A] = {
    val self = this
    CheckF(a =>
      (self(a), that(a)) match {
        case (-\/(e1), -\/(e2)) => (e1 |+| e2).left
        case (-\/(e),  \/-(a))  => e.left
        case (\/-(a),  -\/(e))  => e.left
        case (\/-(a1), \/-(a2)) => a.right
      }
    )
  }

  def apply(a: A): E \/ A =
    f(a)
}
