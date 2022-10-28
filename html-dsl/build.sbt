val testDep = Seq(
  "org.scalactic" %% "scalactic" % "3.2.11",
  "org.scalatest" %% "scalatest" % "3.2.11" % "test"
)

val commonSetting = Seq(
  scalaVersion := "3.2.0",
  resolvers += "Maven Repository" at "https://raw.githubusercontent.com/nanaki13/mvn-repo/main/",
  libraryDependencies += "bon.jo" %% "bon-scala-common" % "1.1.1-SNAPSHOT",
  ThisBuild / publishTo := Some(MavenCache("local-maven", file("I:/work/mvn-repo")))
)
lazy val core = (crossProject(JSPlatform) in file("."))
  .settings(
    name := "html-dsl",
    organization := "bon.jo",
    version := "0.1.2-SNAPSHOT",
    Compile / mainClass := Some(" bon.jo.html.Main")
  )
  .settings(commonSetting)
  .jsSettings(
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.3.0"
  ) 
  .settings(
    libraryDependencies ++= testDep
  )
