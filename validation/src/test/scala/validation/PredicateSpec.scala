package validation

import org.scalatest.{Matchers,WordSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class PredicateSpec extends WordSpec with Matchers with GeneratorDrivenPropertyChecks {
  import cats.data.{NonEmptyList,OneAnd,Validated}
  import cats.std.list._
  import predicate._

  type Pred[A] = Predicate[NonEmptyList[String], A]

  def error(s: String): NonEmptyList[String] =
    OneAnd(s, Nil)

  val even: Pred[Int] =
    Predicate.lift(error("Must be even")){ i => Math.abs(i % 2) == 0 }

  val odd: Pred[Int] =
    Predicate.lift(error("Must be odd")){ i => Math.abs(i % 2) == 1 }

  val noMoreThanMax: Pred[Int] =
    Predicate.lift(error("Must be less than MaxValue")){ _ <= Int.MaxValue }

  val noLessThanMin: Pred[Int] =
    Predicate.lift(error("Must be greater than MinValue")){ _ >= Int.MinValue }

  val greaterThanMax: Pred[Int] =
    Predicate.lift(error("Must be greater than MaxValue")){ _ > Int.MaxValue }

  val lessThanMin: Pred[Int] =
    Predicate.lift(error("Must be less than MinValue")){ _ < Int.MinValue }

  "A Predicate" when {
    "combined with and" must {
      "fail if either component predicate fails" in {
        forAll { (n: Int) =>
          (even and odd)(n).isInvalid should === (true)
        }
      }
      "succeed if both component predicates succeed" in {
        forAll { (n: Int) =>
          (noMoreThanMax and noLessThanMin)(n).isValid should === (true)
        }
      }
    }

    "combined with or" must {
      "fail if both component predicates fail" in {
        forAll { (n: Int) =>
          (greaterThanMax or lessThanMin)(n).isInvalid should === (true)
        }
      }
      "succeed if either component predicate succeeds" in {
        forAll { (n: Int) =>
          (even or odd)(n).isValid should === (true)
        }
      }
    }

    "combined with zip" must {
      "fail if either component predicate fails" in {
        forAll { (m: Int, n: Int) =>
          (greaterThanMax zip noMoreThanMax)( (m,n) ).isInvalid should === (true)
        }
      }
      "succeed if both component predicates succeed" in {
        forAll { (m: Int, n: Int) =>
          (noMoreThanMax zip noLessThanMin)( (m,n) ).isValid should === (true)
        }
      }
    }
  }
}
