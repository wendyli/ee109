scalaVersion in ThisBuild := "2.12.1"

organization := "stanford-ppl"

version := "1.1"

val paradiseVersion = "2.1.0"

publishArtifact := false
trapExit := false

scalaSource in Compile := baseDirectory(_/ "src").value
//libraryDependencies += "stanford-ppl" %% "spatial" % "1.0"

//paradise
resolvers += Resolver.sonatypeRepo("snapshots")
resolvers += Resolver.sonatypeRepo("releases")
addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)
