import java.nio.file.Files
import java.nio.file.Paths

val testDep =      Seq( "org.scalactic" %% "scalactic" % "3.2.11",
     "org.scalatest" %% "scalatest" % "3.2.11" % "test")

val commonSetting = Seq(
  scalaVersion := "3.2.0",
   resolvers += "Maven Repository" at "https://raw.githubusercontent.com/nanaki13/mvn-repo/main/",
    libraryDependencies +=  "bon.jo" %% "bon-scala-common"%"1.1.1-SNAPSHOT",
    libraryDependencies += "bon.jo" %% "user-core" % "1.1.1-SNAPSHOT"
)
lazy val core = (crossProject(JSPlatform, JVMPlatform) in file("core")).settings(
  name := "game-engine",
  organization  := "bon.jo",
  version := "1.1.0-SNAPSHOT"
).settings(commonSetting).
  jvmSettings(
     libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided",
   //  libraryDependencies += "bon.jo" %% "user-core" % "1.1.1-SNAPSHOT",
     libraryDependencies += "bon.jo" %% "bon-sql" % "1.1.1-SNAPSHOT",
     libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.3",
     libraryDependencies += "org.postgresql" % "postgresql" % "42.5.0",
     dockerBaseImage := "openjdk:18" 
  ).enablePlugins(JavaAppPackaging,DockerPlugin).
  jsSettings(
    libraryDependencies += "bon.jo" %%% "html-dsl" % "0.1.1-SNAPSHOT",
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.3.0",
    libraryDependencies += "bon.jo" %%% "user-core" % "1.1.1-SNAPSHOT",
     libraryDependencies += "bon.jo" %%% "bon-scala-common" % "1.1.1-SNAPSHOT",
    
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
lazy val copyStaticToRessources = taskKey[String]("site to res")
copyStaticToRessources := {
  val jsSource =  ** / "*{-opt,-fastopt}.{js,js.map}"
  println(jsSource)
  val jsBuild = Paths.get("""core\js\target\scala-3.1.2""")
  val toCopy = Files.walk(jsBuild).filter(e =>jsSource.matches(e))
 // sbt.io.IO.listFiles(file("""core\js\target\scala-3.1.2""")).filter(e =>jsSource.matches(e.toPath)).foreach(println)
  val targetRes = Paths.get("core\\jvm\\src\\main\\resources\\break-broke")
  sbt.io.IO.delete(targetRes.toFile())
  val targetJs =   targetRes.resolve(jsBuild)
  targetJs.toFile().mkdirs()
  toCopy.forEach(e => {
      println(e)
      sbt.io.IO.copyFile(e.toFile(),targetJs.resolve(e.getFileName()).toFile())
    })
  sbt.io.IO.copyDirectory(file("assets"),targetRes.resolve("assets").toFile())
  sbt.io.IO.copyFile(file("index-prod.html"),targetRes.resolve("index.html").toFile())
  sbt.io.IO.copyFile(file("config-prod.js"),targetRes.resolve("assets/js/config.js").toFile())
  "ok"
}
lazy val copyDockerCompose = taskKey[String]("docker compose to stage")
copyDockerCompose := {
  globFilter("*.yml")
  
  val filter =  ** / "*.{yml}"
  val source = Paths.get("""docker""")
  val toCopy = Files.walk(source).filter(e =>filter.matches(e))
 // sbt.io.IO.listFiles(file("""core\js\target\scala-3.1.2""")).filter(e =>jsSource.matches(e.toPath)).foreach(println)
  val targetRes = Paths.get("""core\jvm\target\docker\stage""")
 

  toCopy.forEach(e => {
      println(e)
      sbt.io.IO.copyFile(e.toFile(),targetRes.resolve(e.getFileName()).toFile())
    })

  "ok"
}

lazy val finalBuild = taskKey[String]("final build")
finalBuild := {
   Def.sequential(core.jvm  / clean,core.js  / clean, ((core.js / Compile / fullOptJS)),
    (copyStaticToRessources),
 ((core.jvm / Compile / compile)),
  ((core.jvm / Docker / stage)),
  (copyDockerCompose)).value

}
