package probability

import scala.language.higherKinds

trait Continuous[F[_]] {
  def normal(mean: Double, variance: Double): F[Double]
}
