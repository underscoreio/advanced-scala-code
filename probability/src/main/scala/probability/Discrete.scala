package probability

import scala.language.higherKinds

trait Discrete[F[_]] {
  def discrete[A](events: List[(A,Probability)]): F[A]

  def pure[A](atom: A): F[A] =
    discrete(List(atom -> 1.0))

  def uniform[A](atoms: List[A]): F[A] = {
    val n = atoms.length
    val weight = 1.0 / n
    discrete(atoms.map(a => a -> weight))
  }

  def bernoulli(weight: Probability): F[Boolean] =
    discrete(List(true -> weight, false -> (1 - weight)))
}
