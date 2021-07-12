name := "StartupScript"

version := "0.1.0"
organization := "org.ergoplatform"
scalaVersion := "2.12.10"

resolvers ++= Seq(
  "Sonatype Public" at "https://oss.sonatype.org/content/groups/public/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "Typesafe maven releases" at "https://repo1.maven.org/maven2/com/typesafe/config/"
)

libraryDependencies ++= Seq(
  "org.ergoplatform" %% "ergo-appkit" % "susy-appkit-local",
  "org.scalaj" %% "scalaj-http" % "2.4.2",
  "com.joefkelley" %% "argyle" % "1.0.0",
  "com.github.scopt" %% "scopt" % "4.0.1",
  "com.typesafe" % "config" % "1.4.1"
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