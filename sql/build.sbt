
val testDep =      Seq( "org.scalactic" %% "scalactic" % "3.2.11",
     "org.scalatest" %% "scalatest" % "3.2.11" % "test")

val commonSetting = Seq(scalaVersion := "3.2.0",libraryDependencies += "bon.jo" %% "bon-scala-common"%"1.1.1-SNAPSHOT",
libraryDependencies += "org.xerial" % "sqlite-jdbc" % "3.36.0.2",
resolvers += "Maven Repository" at "https://raw.githubusercontent.com/nanaki13/mvn-repo/main/",
publishTo := Some(MavenCache("local-maven", file("I:/work/mvn-repo")))
,
scalacOptions ++= Seq(          // use ++= to add to existing options
  "-encoding", "utf8",          // if an option takes an arg, supply it on the same line
  "-feature",  "-deprecation"                 // then put the next option on a new line for easy editing               // exploit "trailing comma" syntax so you can add an option without editing this line
)  
)



lazy val root =( project in file(".")).settings(
  name := "bon-sql",
  version := "1.1.1-SNAPSHOT",
  organization := "bon.jo"

).settings(commonSetting).settings(
    libraryDependencies ++= testDep
  )

