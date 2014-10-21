object TypeClasses {

  trait InvariantShow[A] {
    def show(in: A): String
  }

  trait CovariantShow[+A] {
    def show[AA >: A](in: AA): String
  }

  trait ContravariantShow[-A] {
    def show(in: A): String
  }

}

object Interface {
  import TypeClasses._

  def invariant[A : InvariantShow](in: A): String =
    implicitly[InvariantShow[A]].show(in)

  def covariant[A : CovariantShow](in: A): String =
    implicitly[CovariantShow[A]].show(in)

  def contravariant[A : ContravariantShow](in: A): String =
    implicitly[ContravariantShow[A]].show(in)
}

class Animal()
class Feline() extends Animal
class Moggy() extends Feline

object Instances {
  import TypeClasses._

  implicit object invariantAnimal extends InvariantShow[Animal] {
    def show(in: Animal): String = "Animal"
  }

  implicit object covariantAnimal extends CovariantShow[Animal] {
    def show[AA >: Animal](in: AA): String = "Animal"
  }

  implicit object contravariantAnimal extends ContravariantShow[Animal] {
    def show(in: Animal): String = "Animal"
  }

  implicit object contravariantMoggy extends ContravariantShow[Moggy] {
    def show(in: Moggy): String = "Moggy"
  }
}

object InvariantExample {
  import Instances.invariantAnimal

  def example: String = {
    Interface.invariant(new Animal())
    // Will not compile, because InvariantShow[Animal] is not a
    // subtype of InvariantShow[Feline] or InvariantShow[Moggy]
    //
    //Interface.invariant(new Feline())
    //Interface.invariant(new Moggy())
  }
}

object CovariantExample {
  import Instances.covariantAnimal

  def example: String = {
    Interface.covariant(new Animal())
    // Will not compile, because CovariantShow[Animal] is not a
    // subtype of CovariantShow[Feline] or CovariantShow[Moggy]
    //
    //Interface.covariant(new Feline())
    //Interface.covariant(new Moggy())
  }
}

object ContravariantExample {
  import Instances.contravariantAnimal
  //import Instances.contravariantMoggy

  def example: String = {
    Interface.contravariant(new Animal())
    // Selects contravariantAnimal but if we introduce
    // contravariantMoggy we can't compile due to ambiguity: both
    // instances are applicable
    //
    Interface.contravariant(new Moggy())
  }
}
