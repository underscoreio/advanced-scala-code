package probability

import cats.Monad

final case class Enumeration[A](events: List[(A, Double)]) {
  def flatMap[B](f: A => Enumeration[B]): Enumeration[B] = {
    Enumeration(
      for {
        (a, p1) <- this.events
        (b, p2) <- f(a).events
      } yield b -> (p2 * p1)
    )
  }

  def map[B](f: A => B): Enumeration[B] =
    Enumeration(events.map { case (a, p) => f(a) -> p })

  def normalize: Enumeration[A] = {
    val totalWeight = (events map { case (a, p) => p }).sum
    Enumeration(events map { case (a,p) => a -> (p / totalWeight) })
  }

  def compact: Enumeration[A] = {
    val distinct = (events map { case (a, p) => a }).distinct
    def prob(a: A): Double =
      (events filter { case (x, p) => x == a } map { case (a, p) => p }).sum

    Enumeration(distinct map { a => a -> prob(a) })
  }
}
object Enumeration {
  def pure[A](atom: A): Enumeration[A] =
    discrete(List(atom -> 1.0))

  def uniform[A](atoms: List[A]): Enumeration[A] = {
    val n = atoms.length
    val weight = 1.0 / n
    discrete(atoms.map(a => a -> weight))
  }

  def bernoulli(weight: Probability): Enumeration[Boolean] =
    discrete(List(true -> weight, false -> (1 - weight)))

  def discrete[A](events: List[(A, Double)]): Enumeration[A] =
    Enumeration(events)

  implicit object enumerationInstance extends Monad[Enumeration] with Discrete[Enumeration] {
    override def flatMap[A,B](fa: Enumeration[A])(f: A => Enumeration[B]): Enumeration[B] =
      fa.flatMap(f)

    override def map[A,B](fa: Enumeration[A])(f: A => B): Enumeration[B] =
      fa.map(f)

    override def pure[A](atom: A): Enumeration[A] =
      Enumeration.pure(atom)

    override def discrete[A](events: List[(A, Double)]): Enumeration[A] =
      Enumeration.discrete(events)
  }
}
