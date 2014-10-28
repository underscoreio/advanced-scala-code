import scalaz.{EitherT, WriterT, Monad, \/}
import scalaz.concurrent.Task
import scalaz.syntax.either._
import scalaz.std.vector._

object FutureLogResult {
  type Logged[A] = WriterT[Task, Vector[String], A]
  type Async[B] = EitherT[Logged, String, B]

  object Async {
    def apply[A](in: => A): Async[A] = {
      val value = Task.now(Vector.empty[String] -> in.right)
      val logged: Logged[\/[String, A]] = WriterT(value)
      val async: Async[A] = EitherT(logged)
      async
    }
  }

  implicit object asyncInstance extends Monad[Async] {
    def point[A](a: => A): Async[A] =
      Async(a)

    def bind[A, B](fa: Async[A])(f: A => Async[B]): Async[B] =
      fa.flatMap(f)
  }
}
