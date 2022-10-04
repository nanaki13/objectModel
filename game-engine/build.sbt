
val testDep =      Seq( "org.scalactic" %% "scalactic" % "3.2.11",
     "org.scalatest" %% "scalatest" % "3.2.11" % "test")

val commonSetting = {
  scalaVersion := "3.1.2"

}
lazy val core = (crossProject(JSPlatform, JVMPlatform) in file("core")).settings(
  name := "game-engine",
  organization  := "bon.jo",
  version := "0.1.0-SNAPSHOT"
).settings(commonSetting).
  jvmSettings(
     libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided",
     libraryDependencies += "bon.jo" %% "user-core" % "1.0.0-SNAPSHOT",
     libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.3",
     libraryDependencies += "org.postgresql" % "postgresql" % "42.5.0" 
  ).enablePlugins(JavaAppPackaging,DockerPlugin).
  jsSettings(
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.1.0",
    libraryDependencies += "bon.jo" %%% "user-core" % "1.0.0-SNAPSHOT",
    // Add JS-specific settings here
    scalaJSUseMainModuleInitializer := true
  ).settings(
    libraryDependencies ++= testDep,
    
  )


lazy val root = project.in(file(".")).
  aggregate(core.js, core.jvm).
  settings(
    publish := {},
    publishLocal := {},
    libraryDependencies ++= testDep
  )
