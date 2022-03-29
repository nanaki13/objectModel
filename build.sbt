
val testDep =      Seq( "org.scalactic" %% "scalactic" % "3.2.11",
     "org.scalatest" %% "scalatest" % "3.2.11" % "test")

val commonSetting = {
  scalaVersion := "3.1.1"

}
lazy val core = (crossProject(JSPlatform, JVMPlatform) in file("core")).settings(
  name := "object-core"

).settings(commonSetting).
  jvmSettings(
     libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided"
  ).
  jsSettings(
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.1.0",
    // Add JS-specific settings here
    scalaJSUseMainModuleInitializer := true
  ).settings(
    libraryDependencies ++= testDep
  )


lazy val root = project.in(file(".")).
  aggregate(core.js, core.jvm).
  settings(
    publish := {},
    publishLocal := {},
    libraryDependencies ++= testDep
  )
