name := "MachinaeCoelestis"

version := "0.1.0-SNAPSHOT"

organization := "de.sciss"

homepage <<= name { n => Some(url("https://github.com/Sciss/" + n)) }

licenses := Seq("GPL v3+" -> url("http://www.gnu.org/licenses/gpl-3.0.txt"))

scalaVersion := "2.10.2"

// libraryDependencies ++= Seq(
//    "de.sciss" %% "soundprocesses" % "1.3.+",
//    "de.sciss" %% "lucrestm-bdb" % "1.6.+",
//    "de.sciss" %% "strugatzki" % "1.3.+"
// )

retrieveManaged := true

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

testOptions in Test += Tests.Argument("-oF")

fork in run := true  // required for shutdown hook, and also the scheduled thread pool, it seems

