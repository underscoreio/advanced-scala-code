val scalaz = "org.scalaz" %% "scalaz-core" % "7.1.0"
val scalazConcurrent = "org.scalaz" %% "scalaz-concurrent" % "7.1.0"

val settings = Seq(
  scalaVersion := "2.11.3",
  scalacOptions ++= Seq("-feature"),
  libraryDependencies ++= Seq(scalaz, scalazConcurrent)
)

lazy val monoid = project.settings(settings:_*)
lazy val functor = project.settings(settings:_*)
lazy val monad = project.settings(settings:_*)
lazy val monadTransformer = project.settings(settings:_*)
lazy val applicative = project.settings(settings:_*)
