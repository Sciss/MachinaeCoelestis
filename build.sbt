name := "MachinaeCoelestis"

version := "0.1.0-SNAPSHOT"

organization := "de.sciss"

homepage <<= name { n => Some(url("https://github.com/Sciss/" + n)) }

licenses := Seq("GPL v3+" -> url("http://www.gnu.org/licenses/gpl-3.0.txt"))

scalaVersion := "2.10.3"

resolvers ++= Seq(
  "Mandubian repository snapshots" at "https://github.com/mandubian/mandubian-mvn/raw/master/snapshots/"
//  "Sonatype OSS snapshots"         at "https://oss.sonatype.org/content/repositories/snapshots/"
)

libraryDependencies ++= Seq(
  "de.sciss" %% "mellite"          % "0.5.+",
  "de.sciss" %% "play-json-sealed" % "0.1.+" exclude("org.scala-stm", "scala-stm_2.10.0"), // exclude due to "Conflicting cross-version suffixes"
  "de.sciss" %% "pdflitz"          % "1.0.+",
  "com.github.wookietreiber" %% "scala-chart" % "0.3.0"
)

retrieveManaged := true

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

testOptions in Test += Tests.Argument("-oF")

fork in run := true  // required for shutdown hook, and also the scheduled thread pool, it seems

initialCommands in console := """import de.sciss.coelestis._"""
