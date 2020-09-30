// lazy val hello = taskKey[Unit]("An example task")

lazy val myproject = (project in file("."))  // your existing library
  .settings(name := "scala-with-cats")
  .settings(scalaVersion := "2.12.9")
  .settings(libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0")
  // .settings(hello := { println("Hello!") })


// lazy val docs = project       // new documentation project
//   .in(file("myproject-docs")) // important: it must not be docs/
//   .settings(mdocVariables := Map("VERSION" -> version.value))
//   .dependsOn(myproject)
//   .enablePlugins(MdocPlugin)


scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-Ypartial-unification"
)
