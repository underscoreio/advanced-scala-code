import scalaz.{Reader, Monad}
import scalaz.std.function._

object MonadReaderExample {
  type Id = Int
  final case class User(name: String)
  final case class Db() {
    def getUser(userId: Id) =
      User("John Doe")
    def writeUser(user: User) = ()
  }

  final case class Config(db: Db)
  type Env[A] = Reader[Config, A]

  def getUser(userId: Id): Env[User] =
    Reader(c => c.db.getUser(userId))

  def writeUser(user: User): Env[Unit] =
    Reader(c => c.db.writeUser(user))

  for {
    user <- getUser(1)
    _    <- writeUser(user)
  } yield ()
}
