import sbt._
import Keys._

lazy val versions = Map[String, String](
  "atari-st-tools" -> "0.0.2-SNAPSHOT",
  "config"         -> "1.3.1",
  "logback"        -> "1.1.8",
  "scala"          -> "2.12.1",
  "scala-logging"  -> "3.5.0",
  "scopt"          -> "3.5.0",
  "slf4j"          -> "1.7.21",
  "suiryc-scala"   -> "0.0.2-SNAPSHOT"
)


lazy val atariStTools = project.in(file(".")).
  settings(
    organization := "suiryc",
    name := "atari-st-tools",
    version := versions("atari-st-tools"),
    scalaVersion := versions("scala"),

    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-unchecked",
      "-Xfatal-warnings",
      "-Xlint",
      "-Yno-adapted-args",
      "-Ywarn-numeric-widen",
      "-Ywarn-value-discard",
      "-Ywarn-inaccessible",
      "-Ywarn-infer-any",
      "-Ywarn-dead-code",
      "-Ywarn-nullary-override",
      "-Ywarn-nullary-unit",
      "-Ywarn-unused",
      "-Ywarn-unused-import"
    ),
    scalacOptions in (Compile, doc) ++= Seq("-diagrams", "-implicits"),
    resolvers += Resolver.mavenLocal,

    libraryDependencies ++= Seq(
      "ch.qos.logback"             %  "logback-classic"   % versions("logback"),
      "com.github.scopt"           %% "scopt"             % versions("scopt"),
      "com.typesafe"               %  "config"            % versions("config"),
      "com.typesafe.scala-logging" %% "scala-logging"     % versions("scala-logging"),
      "org.slf4j"                  %  "slf4j-api"         % versions("slf4j"),
      "suiryc"                     %% "suiryc-scala-core" % versions("suiryc-scala")
    ),

    publishMavenStyle := true,
    publishTo := Some(Resolver.mavenLocal)
  )
