lazy val root = (project in file(".")).
  settings(
    name := "hello",
    libraryDependencies += "org.scala-lang" % "scala-swing" % "2.10+"
  )
