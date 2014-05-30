organization := "suiryc"

name := "atari-st-tools"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.10.3"

scalacOptions ++= Seq("-deprecation", "-feature", "-optimize", "-unchecked", "-Yinline-warnings")

scalacOptions in (Compile, doc) ++= Seq("-diagrams", "-implicits")

val versions = Map[String, String](
/* Project dependencies */
  "config" -> "1.2.1",
  "grizzled" -> "1.0.2",
  "logback" -> "1.1.2",
  "scopt" -> "3.2.0",
  "slf4j" -> "1.7.7",
  "suiryc-scala" -> "0.0.1-SNAPSHOT",
/* Maven dependencies */
  "maven-compiler-plugin" -> "3.1",
  "maven-surefire-plugin" -> "2.16",
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

val localMavenPath = Path.userHome.absolutePath + "/.m2/repository"

resolvers += "Local Maven Repository" at "file://" + localMavenPath

seq(Revolver.settings: _*)


publishMavenStyle := true

publishTo := Some(Resolver.file("file", new File(localMavenPath)))

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
          <recompileMode>incremental</recompileMode>
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
          <source>1.8</source>
          <target>1.8</target>
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
