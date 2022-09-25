
val testDep =      Seq( "org.scalactic" %% "scalactic" % "3.2.11",
     "org.scalatest" %% "scalatest" % "3.2.11" % "test")

val commonSetting = Seq(scalaVersion := "3.1.2",libraryDependencies += "org.xerial" % "sqlite-jdbc" % "3.36.0.2")
val AkkaVersion = "2.6.8"
val AkkaHttpVersion = "10.2.9"

lazy val core = (crossProject(JSPlatform, JVMPlatform) in file("core")).settings(
  name := "user-core",
  version := "1.0.0-SNAPSHOT",
  organization := "bon.jo"

).settings(commonSetting).
  jvmSettings(
      libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided",
      libraryDependencies += "com.github.jwt-scala" %% "jwt-json4s-native" % "9.0.5",
      libraryDependencies += "org.json4s" %% "json4s-native" % "4.0.5",
      libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.3" % Runtime,
      libraryDependencies ++= Seq(
      ("com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion).cross(CrossVersion.for3Use2_13),
      ("com.typesafe.akka" %% "akka-stream" % AkkaVersion).cross(CrossVersion.for3Use2_13),
      ("com.typesafe.akka" %% "akka-http" % AkkaHttpVersion).cross(CrossVersion.for3Use2_13)
      )
  ).
  jsSettings(
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.1.0",
    libraryDependencies += "bon.jo" %%% "html-dsl" % "0.1.0-SNAPSHOT",
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
