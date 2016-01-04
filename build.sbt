val scalaz = "org.scalaz" %% "scalaz-core" % "7.1.0"
val scalazConcurrent = "org.scalaz" %% "scalaz-concurrent" % "7.1.0"
val cats = "org.spire-math" %% "cats" % "0.3.0"
val scalaTest = "org.scalatest" %% "scalatest" % "2.2.4" % "test"

val settings = Seq(
  scalaVersion := "2.11.3",
  scalacOptions ++= Seq("-feature"),
  libraryDependencies ++= Seq(scalaz, scalazConcurrent, cats, scalaTest)
)

// Examples
lazy val monoid = project.settings(settings:_*)
lazy val functor = project.settings(settings:_*)
lazy val monad = project.settings(settings:_*)
lazy val monadTransformer = project.settings(settings:_*)
lazy val applicative = project.settings(settings:_*)

// Projects
lazy val validation = project.settings(settings:_*)
