import scalaz.Monoid
import scalaz.syntax.monoid._

object SuperAdder {
  def add[A: Monoid](items: List[A]): A =
    items.foldLeft(mzero[A]){ _ |+| _ }
}

case class Order(unitCost: Double, quantity: Int) {
  val totalCost: Double = unitCost * quantity
}

object Order {
  val zero = Order(0, 0)
  def add(o1: Order, o2: Order): Order =
    Order(
      (o1.totalCost + o2.totalCost) / (o1.quantity + o2.quantity),
      o1.quantity + o2.quantity
    )

  implicit val orderInstance = Monoid.instance[Order](zero, add _)
}

object Examples {
  import scalaz.std.anyVal._
  import scalaz.std.option._

  def ints =
    SuperAdder.add(List(1,2,3,4,5))

  def options =
    SuperAdder.add(List(Some(1), None, Some(2), Some(3), Some(4), None, Some(5)))

  def orders =
    SuperAdder.add(List(
      Order(1, 1),
      Order(2, 2),
      Order(3, 3),
      Order(4, 4),
      Order(5, 5)
    ))
}
