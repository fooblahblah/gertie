import com.typesafe.sbt.SbtStartScript

seq(SbtStartScript.startScriptForClassesSettings: _*)

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

name := "gertie"

organization := "org.fooblahblah"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.10.0"

resolvers ++= Seq(
  "fooblahblah repo" at "https://raw.github.com/fooblahblah/maven-repo/master",
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
  "spray repo" at "http://repo.spray.io",
  "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "releases"  at "http://oss.sonatype.org/content/repositories/releases")

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor"            % "2.1.0",
  "io.spray"          %  "spray-io"              % "1.1-M6",
  "junit"             %  "junit"                 % "4.10" % "test",
  "org.specs2"        %% "specs2"                % "1.13" % "test",
  "org.fooblahblah"   %% "bivouac"               % "1.0.0"
)

initialCommands := "import org.fooblahblah.gertie._"
