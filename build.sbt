name          := "MachinaeCoelestis"
version       := "0.1.0-SNAPSHOT"
organization  := "de.sciss"
homepage      := Some(url(s"https://github.com/Sciss/${name.value}"))
licenses      := Seq("GPL v3+" -> url("http://www.gnu.org/licenses/gpl-3.0.txt"))
scalaVersion  := "2.10.3"

resolvers ++= Seq(
  "Mandubian repository snapshots"  at "https://github.com/mandubian/mandubian-mvn/raw/master/snapshots/",
  "Oracle Repository"               at "http://download.oracle.com/maven", // required for sleepycat
)

libraryDependencies ++= Seq(
  "de.sciss" %% "mellite"          % "0.5.0",
  "de.sciss" %% "play-json-sealed" % "0.4.1", // exclude("org.scala-stm", "scala-stm_2.10.0"), // exclude due to "Conflicting cross-version suffixes"
  "de.sciss" %% "pdflitz"          % "1.2.2",
  "com.github.wookietreiber" %% "scala-chart" % "0.3.0"
  // "de.sciss" % "intensitypalette" % "1.0.0"
)

//retrieveManaged := true

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

testOptions in Test += Tests.Argument("-oF")

fork in run := true  // required for shutdown hook, and also the scheduled thread pool, it seems

initialCommands in console := """import de.sciss.coelestis._"""
