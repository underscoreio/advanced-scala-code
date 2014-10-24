val scalaz = "org.scalaz" %% "scalaz-core" % "7.1.0"

val settings = Seq(
  scalaVersion := "2.11.3",
  libraryDependencies += scalaz
)

lazy val monoid = project.settings(settings:_*)

lazy val monad = project.settings(settings:_*)
