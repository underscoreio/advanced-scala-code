val scalaz = "org.scalaz" %% "scalaz-core" % "7.1.0"
val scalazConcurrent = "org.scalaz" %% "scalaz-concurrent" % "7.1.0"
val cats = "org.spire-math" %% "cats" % "0.4.0-SNAPSHOT"


val settings = Seq(
  scalaVersion := "2.11.6",
  scalacOptions ++= Seq("-feature"),
  libraryDependencies ++= Seq(scalaz, scalazConcurrent, cats),
  resolvers += Resolver.sonatypeRepo("snapshots")
)

lazy val monoid = project.settings(settings:_*)
lazy val functor = project.settings(settings:_*)
lazy val monad = project.settings(settings:_*)
lazy val monadTransformer = project.settings(settings:_*)
lazy val applicative = project.settings(settings:_*)
lazy val pygmyHadoop = project.settings(settings:_*)
lazy val free = project.settings(settings:_*)
lazy val validation = project.settings(settings:_*)
