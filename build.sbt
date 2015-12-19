import sbt._
import Keys._

lazy val versions = Map[String, String](
  "atari-st-tools" -> "0.0.2-SNAPSHOT",
  "config"         -> "1.3.0",
  "grizzled"       -> "1.0.2",
  "logback"        -> "1.1.3",
  "scala"          -> "2.11.7",
  "scopt"          -> "3.2.0",
  "slf4j"          -> "1.7.13",
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
      "-feature",
      "-optimize",
      "-unchecked",
      "-Yinline-warnings"
    ),
    scalacOptions in (Compile, doc) ++= Seq("-diagrams", "-implicits"),
    resolvers ++= Seq(
      Resolver.mavenLocal,
      "spray repo" at "http://repo.spray.io/",
      "spray nightly repo" at "http://nightlies.spray.io"
    ),

    libraryDependencies ++= Seq(
      "ch.qos.logback"   %  "logback-classic"   % versions("logback"),
      "com.github.scopt" %% "scopt"             % versions("scopt"),
      "com.typesafe"     %  "config"            % versions("config"),
      "org.clapper"      %% "grizzled-slf4j"    % versions("grizzled"),
      "org.slf4j"        %  "slf4j-api"         % versions("slf4j"),
      "suiryc"           %% "suiryc-scala-core" % versions("suiryc-scala")
    ),

    publishMavenStyle := true,
    publishTo := Some(Resolver.mavenLocal)
  )
