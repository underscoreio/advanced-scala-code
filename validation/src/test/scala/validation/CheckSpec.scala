package validation

import org.scalatest.{Inside,Matchers,WordSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class CheckSpec extends WordSpec with Matchers with GeneratorDrivenPropertyChecks with Inside {
  import cats.data.{NonEmptyList,OneAnd,Validated}
  import cats.std.list._
  import check._
  import predicate._

  type Pred[A] = Predicate[NonEmptyList[String], A]

  type Error = NonEmptyList[String]
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

  "A Check" when {
    "applied to a value" must {
      "fail if its component predicate fails" in {
        forAll { (n: Int) =>
          Check(even and odd)(n).isInvalid should === (true)
        }
      }
      "succeed if component predicate succeeds" in {
        forAll { (n: Int) =>
          Check(noMoreThanMax and noLessThanMin)(n).isValid should === (true)
        }
      }

      "apply any map" in {
        forAll { (n: Int) =>
          val result = (Check(even or odd) map (n => n + 42))(n)
          inside(result) { case Validated.Valid(i) =>
            i should === (n + 42)
          }
        }
      }

      "apply any flatMap" in {
        forAll { (n: Int) =>
          val check =
            Check(even or odd) flatMap { i =>
              if(i < 0)
                Check(noMoreThanMax) map { i => "negative" }
              else
                Check(noLessThanMin) map { i => "not negative" }
            }

          inside(check(n)) { case Validated.Valid(s) =>
            s should (equal ("negative") or equal ("not negative"))
          }
        }
      }
    }
  }
}
