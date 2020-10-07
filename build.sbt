// lazy val hello = taskKey[Unit]("An example task")

lazy val myproject = (project in file("."))  // your existing library
  .settings(name := "scala-with-cats")
  .settings(scalaVersion := "2.13.0")

val catsVersion = "2.2.0"
libraryDependencies ++= Seq(
  "org.typelevel"  %% "cats-core" % catsVersion,
  "org.scalactic"  %% "scalactic" % "3.2.0",
  "org.scalatest"  %% "scalatest" % "3.2.0" % "test",
  "org.typelevel"  %% "cats-laws" %  catsVersion % "test",
  "org.typelevel"  %% "cats-testkit" % catsVersion % "test",
  "org.typelevel"  %% "cats-testkit-scalatest" % "2.0.0" % "test",
  "org.typelevel"  %% "discipline-core" % "1.0.0" % "test",
  "org.typelevel"  %% "discipline-specs2" % "1.0.0" % "test",
  "org.typelevel"  %% "discipline-scalatest" % "1.0.0" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.1",
  "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.3" % Test
)

scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-language:higherKinds",
)
