lazy val root = (project in file(".")).
  settings(
    name := "EvComp",
    libraryDependencies += "org.scala-lang" % "scala-swing" % "2.10+",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.5+" % "test",
    scalacOptions += "-feature",
    scalacOptions += "-language:implicitConversions",
    scalacOptions += "-language:postfixOps",
    mainClass in (Compile, run) := Some("com.App"),
    mainClass in assembly := Some("com.Experiment"),
    assemblyJarName in assembly := "Experiment.jar"
  )
