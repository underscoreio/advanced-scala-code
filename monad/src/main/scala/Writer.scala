import scalaz.Writer
import scalaz.syntax.writer._
import scalaz.std.vector._

object WriterExamples {
  def example = {
    val writer = for {
      v <- 42.set(Vector("The answer"))
      _ <- Vector("Just log something").tell
      w <- (v + 1).set(Vector("One more than the answer"))
    } yield w

    writer.written.map(println)
  }
}
