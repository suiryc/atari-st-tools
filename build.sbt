organization := "suiryc"

name := "atari-st-tools"

version := "0.0.2-SNAPSHOT"

scalaVersion := "2.11.1"

scalacOptions ++= Seq("-deprecation", "-feature", "-optimize", "-unchecked", "-Yinline-warnings")

scalacOptions in (Compile, doc) ++= Seq("-diagrams", "-implicits")

val versions = Map[String, String](
  "java" -> "1.8",
/* Project dependencies */
  "config" -> "1.2.1",
  "grizzled" -> "1.0.2",
  "logback" -> "1.1.2",
  "scopt" -> "3.2.0",
  "slf4j" -> "1.7.7",
  "suiryc-scala" -> "0.0.2-SNAPSHOT",
/* Maven dependencies */
  "maven-compiler-plugin" -> "3.1",
  "maven-surefire-plugin" -> "2.17",
  "scala-maven-plugin" -> "3.1.6"
)

libraryDependencies ++= Seq(
  "com.typesafe" % "config" % versions("config"),
  "org.clapper" %% "grizzled-slf4j" % versions("grizzled"),
  "ch.qos.logback" % "logback-classic" % versions("logback"),
  "com.github.scopt" %% "scopt" % versions("scopt"),
  "org.slf4j" % "slf4j-api" % versions("slf4j"),
  "suiryc" %% "suiryc-scala-core" % versions("suiryc-scala")
)

resolvers += Resolver.mavenLocal

Seq(Revolver.settings: _*)


publishMavenStyle := true

publishTo := Some(Resolver.mavenLocal)

pomExtra := (
  <properties>
    <encoding>UTF-8</encoding>
  </properties>
  <build>
    <sourceDirectory>src/main/scala</sourceDirectory>
    <testSourceDirectory>src/test/scala</testSourceDirectory>
    <plugins>
      <plugin>
        <groupId>net.alchim31.maven</groupId>
        <artifactId>scala-maven-plugin</artifactId>
        <version>{ versions("scala-maven-plugin") }</version>
        <configuration>
          <args>
            <arg>-deprecation</arg>
            <arg>-feature</arg>
            <arg>-Yinline-warnings</arg>
            <arg>-optimize</arg>
            <arg>-unchecked</arg>
          </args>
        </configuration>
        <executions>
          <execution>
            <goals>
              <goal>compile</goal>
              <goal>testCompile</goal>
            </goals>
          </execution>
        </executions>
      </plugin>
      <plugin>
        <groupId>org.apache.maven.plugins</groupId>
        <artifactId>maven-compiler-plugin</artifactId>
        <version>{ versions("maven-compiler-plugin") }</version>
        <configuration>
          <source>{ versions("java") }</source>
          <target>{ versions("java") }</target>
        </configuration>
      </plugin>
      <plugin>
        <groupId>org.apache.maven.plugins</groupId>
        <artifactId>maven-surefire-plugin</artifactId>
        <version>{ versions("maven-surefire-plugin") }</version>
        <configuration>
          <includes>
            <include>**/*Suite.class</include>
          </includes>
        </configuration>
      </plugin>
    </plugins>
  </build>
)
