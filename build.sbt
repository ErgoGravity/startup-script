name := "StartupScript"

version := "0.1.0"
organization := "ergo"
scalaVersion := "2.12.10"

resolvers ++= Seq(
  "Sonatype Public" at "https://oss.sonatype.org/content/groups/public/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "Typesafe maven releases" at "https://dl.bintray.com/typesafe/maven-releases/"
)

libraryDependencies ++= Seq(
  "org.ergoplatform" %% "ergo-appkit" % "4.0.4",
  "com.joefkelley" %% "argyle" % "1.0.0",
  "com.github.scopt" %% "scopt" % "4.0.1"
)

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-language:experimental.macros",
  "-language:higherKinds",
  "-language:postfixOps",
  "-feature",
  "-unchecked",
  "-Xfuture",
  "-Yno-adapted-args",
  "-Ywarn-numeric-widen",
  "-Ypartial-unification"
)

test in assembly := {}

assemblyJarName in assembly := s"${name.value}-${version.value}.jar"

assemblyMergeStrategy in assembly := {
  case "logback.xml" => MergeStrategy.first
  case "module-info.class" => MergeStrategy.discard
  case other => (assemblyMergeStrategy in assembly).value(other)
}