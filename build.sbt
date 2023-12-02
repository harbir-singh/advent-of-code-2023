lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code-2023",
    scalaVersion := "2.13.10",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.9.0",
      "org.typelevel" %% "cats-effect" % "3.4.11",
    ),
  )
