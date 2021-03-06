name := "cpalgo"

organization := "com.github.antma"

version := "1.0"

scalaVersion := "2.13.2"

libraryDependencies ++= List(
  "org.specs2"             %% "specs2-core"              % "4.7.0" % "test"
)

scalacOptions ++= Seq(
  "-language:implicitConversions",
  "-language:postfixOps",
  "-feature",
  "-unchecked",
  "-deprecation",
  "-Xlint:_",
  "-Ywarn-macros:after",
  "-Ywarn-unused:_",
  "-Xmaxerrs",
  "12",
  "-Xmaxwarns",
  "12"
)

