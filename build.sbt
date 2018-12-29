name := "functional-actors"
version := "0.0.1-SNAPSHOT"

scalaVersion := "2.12.7"

scalacOptions ++= Seq(
  "-encoding", "UTF-8",   // source files are in UTF-8
  "-deprecation",         // warn about use of deprecated APIs
  "-unchecked",           // warn about unchecked type parameters
  "-feature",             // warn about misused language features
  "-language:higherKinds",// allow higher kinded types without `import scala.language.higherKinds`
  "-Xlint",               // enable handy linter warnings
//  "-Xfatal-warnings",     // turn compiler warnings into errors
  "-Ypartial-unification" // allow the compiler to unify type constructors of different arities
)


libraryDependencies ++= {
  val LogbackVersion = "1.2.3"
  val log4CatsVersion = "0.2.0"

  Seq(
    "co.fs2" %% "fs2-core" % "1.0.0",
    "ch.qos.logback" % "logback-classic" % LogbackVersion,
    "io.chrisdavenport" %% "log4cats-core" % log4CatsVersion,
    "io.chrisdavenport" %% "log4cats-slf4j" % log4CatsVersion)
}


addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
