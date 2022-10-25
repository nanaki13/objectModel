
val testDep =      Seq( "org.scalactic" %% "scalactic" % "3.2.11",
     "org.scalatest" %% "scalatest" % "3.2.11" % "test")

val commonSetting = Seq(scalaVersion := "3.2.0",
libraryDependencies ++= Seq("org.xerial" % "sqlite-jdbc" % "3.36.0.2"),
//resolvers += "Local Maven Repository" at "file://I:/work/mvn-repo",
resolvers += "Maven Repository" at "https://raw.githubusercontent.com/nanaki13/mvn-repo/main/",
publishTo := Some(MavenCache("local-maven", file("I:/work/mvn-repo"))),
scalacOptions ++= Seq(          // use ++= to add to existing options
  "-encoding", "utf8",          // if an option takes an arg, supply it on the same line
  "-feature",  "-deprecation"                 // then put the next option on a new line for easy editing               // exploit "trailing comma" syntax so you can add an option without editing this line
) 
)
val AkkaVersion = "2.6.8"
val AkkaHttpVersion = "10.2.9"

lazy val core = (crossProject(JSPlatform, JVMPlatform) in file("core")).settings(
  name := "user-core",
  version := "1.1.1-SNAPSHOT",
  organization := "bon.jo"

).settings(commonSetting).
  jvmSettings(
      libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided",
      libraryDependencies += "com.github.jwt-scala" %% "jwt-json4s-native" % "9.0.5",
      libraryDependencies += "org.json4s" %% "json4s-native" % "4.0.5",
      libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.3" ,
      libraryDependencies += "bon.jo" %% "bon-scala-common"%"1.1.1-SNAPSHOT",
      libraryDependencies += "bon.jo" %% "bon-sql"%"1.1.1-SNAPSHOT",
      libraryDependencies += ("com.github.t3hnar" %% "scala-bcrypt" % "4.3.0").cross(CrossVersion.for3Use2_13),
      libraryDependencies ++= Seq(
      ("com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion).cross(CrossVersion.for3Use2_13),
      ("com.typesafe.akka" %% "akka-stream" % AkkaVersion).cross(CrossVersion.for3Use2_13),
      ("com.typesafe.akka" %% "akka-http" % AkkaHttpVersion).cross(CrossVersion.for3Use2_13)
      ),

  ).enablePlugins(JavaAppPackaging,DockerPlugin).
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
